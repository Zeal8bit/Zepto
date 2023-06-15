; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    INCLUDE "zos_video.asm"

    DEFC EDITOR_CHARS_COLOR      = (TEXT_COLOR_BLACK << 8) | TEXT_COLOR_WHITE
    DEFC EDITOR_BACKGROUND_COLOR = (TEXT_COLOR_WHITE << 8) | TEXT_COLOR_BLACK
    DEFC EDITOR_FOOTER_HEIGHT = 3

    EXTERN _buffer_previous_ath_line_address
    EXTERN model_get_current_line
    EXTERN model_next_line_address
    EXTERN model_get_remaining_lines
    EXTERN model_get_bc_left_chars
    EXTERN model_get_bc_right_chars
    EXTERN model_move_cursor_left_to
    EXTERN strlen
    EXTERN strncpy
    EXTERN basename

    SECTION TEXT


    ;=========================================================;
    ;=====================  VIEW =============================;
    ;=========================================================;

    ; Parameters:
    ;   DE - String containing the file name to open, NULL else
    PUBLIC editor_view_init
editor_view_init:
    ; Copy the filename
    ld a, d
    or e
    jr z, _editor_view_init_no_file
    ex de, hl
    call basename
    ; Basename in HL
    ld de, _editor_filename
    ld bc, 16
    call strncpy
_editor_view_init_no_file:
    ld a, 80
    ld (_editor_screen_width), a
    ld a, 40
    ld (_editor_screen_height), a
    ; Absolute X coordinate for lines
    ld hl, 0
    ld (_file_cursor_x), hl
    ; Draw the initial view
    call _editor_draw_init_screen
    ; Set X and Y to (0, 1) at the same time, cursor_y comes first
    ld de, 1
    ld (_cursor_y), de
    jp _editor_view_restore_cursor_no_load


_editor_name: DEFM "ZOS Zepto "
              INCBIN "version.txt"
_editor_name_end:
_editor_filename: DEFS 17, "New_File" ; \0 will be added automatically
    ; The welcome message must NOT be bigger than the screen width or it will corrupt other data
_editor_welcome_msg: DEFM "Welcome to zepto. For help, type Ctrl+H", 0
_editor_shortcuts_msg:
    DEFM "^H Help   ^L Location    ^X Cut         ^C Copy        ^V Paste                 \n"
    DEFM "^K Exit   ^S Save File   ^O Open File   ^N New File                             "
_editor_shortcuts_msg_end:


    ; Routine to draw the initial screen, with the top menu and the options at the bottom
    nop
_editor_draw_init_screen:
    ld de, EDITOR_BACKGROUND_COLOR
    call _editor_draw_set_color
    ; Set the cursor to the top of the screen and start drawing
    ld de, 0
    call _editor_view_restore_cursor_no_load
    ; Print the file name or "New file*" in the absence of file name
    call _editor_view_erase_buffer
    push bc ; Save the screen width
    ld hl, _editor_filename
    xor a ; not a notification
    call _editor_copy_message
    ; Print the name of the program at the top left corner
    ld de, _editor_view_buffer + 1
    ld hl, _editor_name
    ld bc, _editor_name_end - _editor_name
    ldir
    ld de, _editor_view_buffer
    pop bc
    inc bc
    call _editor_draw_line
    ; Let's print this buffer once, prepare the length
    ld de, EDITOR_CHARS_COLOR
    call _editor_draw_set_color
    call _editor_view_erase_buffer
    ; Add a newline at the end of the buffer
    ld a, '\n'
    ld (de), a
    ld de, _editor_view_buffer
    ; Then print height - (footer + 1) lines
    ld a, (_editor_screen_height)
    sub EDITOR_FOOTER_HEIGHT + 1
    ; Put this amount in L and H
    ld h, a
    ld l, a
    ; Print the \n that follows the buffer
    inc bc
_editor_draw_init_lines_loop:
    call _editor_draw_line
    dec l
    jp nz, _editor_draw_init_lines_loop
    ; Now we have to print the footer, set the background colors again
    ld de, EDITOR_BACKGROUND_COLOR
    call _editor_draw_set_color
    ; Before calling editor_show_notification, set the cursor_y position that will be set
    ; at the end of the routine (_cursor_x was already set to 0 by the loader)
    ld a, h
    add 2
    ld (_cursor_y), a
    ld hl, _editor_welcome_msg
    call editor_show_notification
    ; Print the shortcuts
    S_WRITE3(DEV_STDOUT, _editor_shortcuts_msg, _editor_shortcuts_msg_end - _editor_shortcuts_msg)
    ; And reset the colors
    ld de, EDITOR_CHARS_COLOR
    jp _editor_draw_set_color


    ; Write a message in the notification zone
    ; Parameters:
    ;   HL - Message to write
    ; Returns:
    ;   None
    PUBLIC editor_show_notification
editor_show_notification:
    push hl
    call _editor_view_erase_buffer
    ; Save the length of the buffer to print on the stack
    ld h, b
    ld l, c
    ex (sp), hl
    ld a, 1
    call _editor_copy_message
    ; Set the position of the cursor to the notification zone
    ld a, (_editor_screen_height)
    sub EDITOR_FOOTER_HEIGHT
    ld d, 0
    ld e, a
    call _editor_view_restore_cursor_no_load
    ; Set the color of the notification text
    ld de, EDITOR_BACKGROUND_COLOR
    call _editor_draw_set_color
    ; Print the buffer, pop its size from the stack
    pop bc
    ld de, _editor_view_buffer
    S_WRITE1(DEV_STDOUT)
    ; Restore the current text color
    ld de, EDITOR_CHARS_COLOR
    call _editor_draw_set_color
    ; Restore the cursor position and return
    jp _editor_view_restore_cursor


    ; Copy a message to the message buffer, it will be copied at the middle
    ; Parameters:
    ;   HL - Message to copy
    ;    A - 1 if notification ([] appended), 0 else
_editor_copy_message:
    push af
    call strlen
    ; B is 0 for sure here, make sure it is not bigger than the width - 4 (for brackets)
    ld a, (_editor_screen_width)
    sub 4
    cp c
    jr nc, _editor_copy_message_size_ok
    ld c, a
_editor_copy_message_size_ok:
    ; DE = _editor_view_buffer + (size/2 - name_len/2)
    ld a, c
    srl a
    adc 0
    ld d, a
    ld a, (_editor_screen_width)
    srl a
    sub d
    ; DE += A
    ld de, _editor_view_buffer
    add e
    ld e, a
    adc d
    sub e
    ld d, a
    ; Copy the name and finally copy the line
    pop af
    or a
    jr z, _editor_copy_message_no_brack
    ; Go backward to add the '[', the buffer was cleaned with spaces, no need to write a space again
    ex de, hl
    dec hl
    dec hl
    ld (hl), '['
    inc hl
    inc hl
    ex de, hl
_editor_copy_message_no_brack:
    ldir
    or a
    ret z
    ex de, hl
    inc hl
    ld (hl), ']'
    ret


    ; Returns:
    ;   BC - Width of the screen
    ;   HL - Address of the last character
    ;   DE - Address of the last character + 1
    ; Alters:
    ;   BC, HL, DE
_editor_view_erase_buffer:
    ; Initialize the temporary buffer with spaces first
    ld bc, (_editor_screen_width)
    push bc
    ld hl, _editor_view_buffer
    ld (hl), ' '
    ld d, h
    ld e, l
    inc de
    dec bc
    ldir
    pop bc
    ret


    ; Parameters:
    ;   DE - Line to print
    ;   BC - Length of the line to print
    ; Returns:
    ;   A - ERR_SUCCESS on success, error code else
    ; Alters:
    ;   A
_editor_draw_line:
    push hl
    push bc
    S_WRITE1(DEV_STDOUT)
    pop bc
    pop hl
    ret


    ; Set the current colors
    ; Parameter:
    ;   DE - Background and foreground colors
_editor_draw_set_color:
    push hl
    push bc
    ld h, DEV_STDOUT
    ld c, CMD_SET_COLORS
    IOCTL()
    pop bc
    pop hl
    ret



    ; Adjust the value of BC to be the minimum between BC and screen width
    ; Parameters:
    ;   BC - Value to test
    ; Returns:
    ;   BC - MIN(BC,_editor_screen_width)
    ; Alters:
    ;   A
editor_adjust_bc_width:
    ld a, b
    or a
    jr nz, _editor_adjust_bc_big
    ld a, (_editor_screen_width)
    cp c
    ret nc
_editor_adjust_bc_big:
    ld bc, (_editor_screen_width)
    ret


    ; Calculate the minimum between HL and BC and put the result in BC
    ; Parameters:
    ;   HL - 16-bit value
    ;   BC - 16-bit value
    ; Returns:
    ;   BC - Minimum between HL and BC
    ; Alters:
    ;   HL, BC
editor_min_hl_bc:
    or a    ; Clear carry flag
    sbc hl, bc
    ret nc
    ; HL was the minimum, restore it and store it in BC
    add hl, bc
    ld b, h
    ld c, l
    ret

    ; Calculate the remaining size on the screen line
    ; Parameters:
    ;   -
    ; Returns:
    ;   A - Remaining size on screen line
    ; Alters:
    ;   A, L
editor_line_rem_size:
    ld a, (_cursor_x)
    ld l, a
    ld a, (_editor_screen_width)
    sub l
    ret



    ; Print a line at the cursor location and go to the next line afterwards.
    ; This is used when loading a file, in order to show it on screen.
    ; Parameters:
    ;   HL - Address of the line to show
    ;   BC - Length of the line to print
    PUBLIC _editor_view_init_new_line
_editor_view_init_new_line:
    call editor_adjust_bc_width
    ex de, hl
    S_WRITE1(DEV_STDOUT)
    ; Print a new line at the end
    ld hl, _editor_view_buffer
    ld (hl), '\n'
    ex de, hl
    ld c, 1
    S_WRITE1(DEV_STDOUT)
    ret


    ; Clean the whole string of size BC at cursor, DO NOT modify the cursor
    ; This shall be called when a new line is inserted in the middle of a line.
    ; Parameters:
    ;   BC - Size to clean
    PUBLIC _editor_view_clean_string_at_cursor
_editor_view_clean_string_at_cursor:
    ret

    ; Restore the cursor to its original position
    PUBLIC _editor_view_restore_cursor
_editor_view_restore_cursor:
    ld de, (_cursor_y)
_editor_view_restore_cursor_no_load:
    ld c, CMD_SET_CURSOR_XY
    ld h, DEV_STDOUT
    IOCTL()
    ret


    ; Increment both the view cursor as well as the absolute cursor
    ; Parameters:
    ;   -
    ; Returns:
    ;   -
editor_view_increment_x:
    ld hl, (_file_cursor_x)
    inc hl
    ld (_file_cursor_x), hl
    ld hl, _cursor_x
    inc (hl)
    ret


    ; Decrement both the view cursor as well as the absolute cursor
    ; Parameters:
    ;   -
    ; Returns:
    ;   -
editor_view_decrement_x:
    ld hl, (_file_cursor_x)
    dec hl
    ld (_file_cursor_x), hl
    ld hl, _cursor_x
    dec (hl)
    ret


    ; Checks if the current line is a long line (requires horizontal scrolling)
    ; Parameters:
    ;   -
    ; Returns:
    ;   NZ flag - Is a long line
    ;   Z - Not a long line
    ; Alters:
    ;   A, HL, DE
editor_view_long_line:
    ld hl, (_file_cursor_x)
    ld de, (_editor_screen_width)
    or a
    ; X cursor starts at index 0, increment it to get the actual length
    inc hl
    sbc hl, de
    ; If no carry:
    ;  Z is set - the line is not longer than the width
    ;  NZ is not set - the line is longer than the width
    ret nc
    ; On carry, the width is bigger than the cursor, set Z flag
    xor a
    ret


    ; Clean the current line and position the cursor at the beginning
    ; Parameters:
    ;   -
    ; Returns:
    ;   -
    ; Alters:
    ;   A, BC, DE, HL
_editor_view_line_clean_beginning:
    xor a
    ld (_cursor_x), a
    call _editor_view_restore_cursor
    call _editor_view_erase_buffer
    ld de, _editor_view_buffer
    S_WRITE1(DEV_STDOUT)
    call _editor_view_restore_cursor
    ret


    ; Print a whole string at current cursor position without moving it. After printing, move
    ; it to the right once.
    ; At the end the cursor must be at original position + 1.
    ; Parameters:
    ;   HL - String to print
    ;   BC - Size of the string (guaranteed > 1)
    PUBLIC _editor_view_insert_string_at_cursor
_editor_view_insert_string_at_cursor:
    ex de, hl
    ; Set BC to the minimum between BC and the remaining size on this line on screen
    call editor_line_rem_size
    ; If there is 1 space left on screen, we will reach the border after writing the current one
    dec a
    jr z, _editor_view_insert_string_at_cursor_overflow
    inc a
    ld l, a
    ld h, 0
    call editor_min_hl_bc
_editor_view_insert_string_at_cursor_print:
    S_WRITE1(DEV_STDOUT)
    call editor_view_increment_x
    jp _editor_view_restore_cursor
    ; Jump to the following label if the cursor is at the end of the line
    ; Clear the whole line, and make the cursor jump back to the beginning of the line
_editor_view_insert_string_at_cursor_overflow:
    push de
    push bc
    call _editor_view_line_clean_beginning
    pop bc
    pop de
    jp _editor_view_insert_string_at_cursor_print


    ; Print the character given at the current cursor location,
    ; reset the cursor (i.e. switch back to characters colors).
    ; All checks have been performed already by the buffer/model.
    ; Parameters:
    ;   A - Character to print
    PUBLIC _editor_view_insert_at_cursor
_editor_view_insert_at_cursor:
    ; D is not altered by the routine below
    ld d, a
    call editor_line_rem_size
    ; If there is only a single spot on the screen line remaining, scroll to the right
    dec a
    jr z, _editor_view_insert_at_cursor_overflow
    ld a, d
_editor_view_insert_at_cursor_print:
    ld de, _editor_view_buffer
    ld (de), a
    ld bc, 1
    S_WRITE1(DEV_STDOUT)
    jp editor_view_increment_x
_editor_view_insert_at_cursor_overflow:
    push de
    call _editor_view_line_clean_beginning
    pop af
    ; Increment the absolute cursor here too, in total it needs to be incremented twice:
    ;   - Once when we switch screen
    ;   - Once after inserting the new character
    ld hl, (_file_cursor_x)
    inc hl
    ld (_file_cursor_x), hl
    jp _editor_view_insert_at_cursor_print


    ; Print a whole string at cursor - 1, and update the cursor.
    ; Routine called when a character is removed from the middle of a
    ; line. So also clear the character after printing.
    ; Parameters:
    ;   HL - String to print
    ;   BC - Size of the string
    ; Alters:
    ;   BC, DE, HL
    PUBLIC _editor_view_insert_string_before_cursor
_editor_view_insert_string_before_cursor:
    push hl
    push bc
    ; Move the cursor left, it will perform the horizontal scroll if needed
    call _editor_view_left_cursor
    pop bc
    ; If the cursor is at the end of the screen, we need to print the next
    ; character below it
    call editor_line_rem_size
    dec a
    jr z, _editor_view_insert_string_before_cursor_next_char
    ; Regular insertion at cursor, we are not at the edge of the screen
    inc a
    ld l, a
    ld h, 0
    call editor_min_hl_bc
    pop hl
    ; We can print BC characters on screen, copy it to our buffer first, because
    ; we need to append a ' ' to clean the last character on the line
    push bc
    ld de, _editor_view_buffer
    ldir
    ld a, ' '
    ld (de), a
    pop bc
    ; We just added a space to the string, increment the size
    inc bc
    ld de, _editor_view_buffer
    S_WRITE1(DEV_STDOUT)
    jp _editor_view_restore_cursor
_editor_view_insert_string_before_cursor_next_char:
    pop hl
    ld de, _editor_view_buffer
    ldi
    ; Continue by adding a backspace character
    ld a, '\b'
    ld (de), a
    dec de
    ; Print out these chars
    ld bc, 2
    S_WRITE1(DEV_STDOUT)
    ; The cursors have been updated already in _editor_view_left_cursor routine
    ret


    ; Remove the character on the view. The cursor must be moved to the left.
    PUBLIC _editor_view_remove_at_cursor
_editor_view_remove_at_cursor:
    ; Simplify this function by moving left first
    call _editor_view_left_cursor
    ; Replace the character under the cursor, do not update the cursors anymore
    ld hl, _editor_view_buffer
    ld (hl), ' '
    inc hl
    ld (hl), '\b'
    ; Reposition HL and put it in DE
    dec hl
    ex de, hl
    ld bc, 2
    S_WRITE1(DEV_STDOUT)
    ret


    ; Move the cursor left, all checks have been performed by
    ; the model.
    PUBLIC _editor_view_left_cursor
_editor_view_left_cursor:
    ; If the X cursor is 0, it means we are currently on a long line, we need to
    ; horizontally scroll to the left
    ld a, (_cursor_x)
    ; Check if A is 1
    dec a
    jr z, _editor_view_left_cursor_check_scroll
_editor_view_left_cursor_regular:
    ; Else, no scroll, we can decrement both cursors
    call editor_view_decrement_x
    jp _editor_view_restore_cursor
_editor_view_left_cursor_check_scroll:
    ; Check if we need to go to the line left content. This is the case when
    ; the file cursor is NOT 1
    ld hl, (_file_cursor_x)
    dec hl
    ; Check if HL is 0, if yes, we are not on a long line
    ld a, h
    or l
    jp z, _editor_view_left_cursor_regular
    ; We are on a long line, load the left content!
    ; Decrement HL again as we are in fact moving the cursor twice
    dec hl
    ld (_file_cursor_x), hl
    ; Get the last screen width - 1 characters from the model
    ld bc, (_editor_screen_width)
    dec c
    ; Take advantage of the fact that BC is set to set the X cursor on screen
    ld a, c
    ld (_cursor_x), a
    ; Get the previous BC bytes from where the cursor is
    call model_get_bc_left_chars
    ; The cursor is not at X = 0, we can do a syscall for thi, but it will add
    ; an overhead, so, a faster way is to insert a \b before the buffer.
    ; Of course, we have to save its content first
    dec hl
    ld a, (hl)
    ld (hl), '\b'
    ; We jus added a new char, increment BC
    inc bc
    ex de, hl
    push af
    S_WRITE1(DEV_STDOUT)
    ; Restore the -1st byte of the buffer
    pop af
    ld (de), a
    ; Print the character that must be under the cursor
    ; FIXME: Use a '>' to show that there are more characters hiding?
    ld bc, 1
    call model_get_bc_right_chars
    ex de, hl
    S_WRITE1(DEV_STDOUT)
    jp _editor_view_restore_cursor


    ; Move the cursor right
    PUBLIC _editor_view_right_cursor
_editor_view_right_cursor:
    ld hl, _cursor_x
    ; If the cursor has reached the end of the line (last character), we have to
    ; horizontally scroll the content.
    call editor_line_rem_size
    ; If there is 1 space left on screen, we will reach the border after writing the current one
    dec a
    jr z, _editor_view_right_cursor_overflow
_editor_view_right_cursor_no_overflow:
    ; No overflow, we can simply move X cursor right (increment)
    call editor_view_increment_x
    jp _editor_view_restore_cursor
_editor_view_right_cursor_overflow:
    ; In total we will increment the absolute X cursor twice, because we have to print
    ; back the last character on the first column.
    call editor_view_increment_x
    ; Clean the buffer line, set the visual X cursor to 0
    call _editor_view_erase_buffer
    ; Reposition the cursor to the beginning of the screen
    xor a
    ld (_cursor_x), a
    call _editor_view_restore_cursor
    ; Get the character that is right before the cursor and the characters that are
    ; right after, put them inside our own buffer.
    ld bc, 1
    call model_get_bc_left_chars
    ld de, _editor_view_buffer
    ldi
    ; DE points to the rest of the buffer
    ld bc, (_editor_screen_width)
    push bc
    dec c   ; the width is in fact an 8-bit value
    call model_get_bc_right_chars
    ; Copy them into our buffer now (BC has been adjusted)
    ldir
    ; Print our buffer now
    pop bc
    ld de, _editor_view_buffer
    S_WRITE1(DEV_STDOUT)
    jp _editor_view_right_cursor_no_overflow


    ; Check if the cursor is at the bottom of the text area
    ; Parameters:
    ;   -
    ; Returns:
    ;   Z flag  - Bottom of text area
    ;   NZ flag - Not at the bottom of text area
    ; Alters:
    ;   A, D
_editor_cursor_bottom_line:
    ld a, (_cursor_y)
    ld d, a
    ld a, (_editor_screen_height)
    ; The text area is smaller than the screen height because of the header and footer
    sub EDITOR_FOOTER_HEIGHT + 1
    cp d
    ret


    ; Copy a given string to the given buffer.
    ; If the string is smaller than the buffer, the latter will be stuffed with spaces.
    ; At most _editor_screen_width characters will be written to the buffer.
    ; Parameters:
    ;   HL - Source string address
    ; Returns:
    ;   BC - Total bytes written
_editor_copy_string_to_buffer:
    ; Copy the lines and stuff the buffer with spaces
    ld bc, (_editor_screen_width)
    push bc
    ld de, _editor_view_buffer
    ld b, c
    ld a, '\n'
_editor_copy_string_to_buffer_fst_loop:
    cp (hl)
    jr z, _editor_copy_string_to_buffer_stuff
    ; Use LDI but don't use BC as the size, simply use B
    ldi
    djnz _editor_copy_string_to_buffer_fst_loop
    ; No more space in the buffer, return it
    jp _editor_copy_string_to_buffer_ret
_editor_copy_string_to_buffer_stuff:
    ; Write the remaining characters
    ld a, ' '
_editor_copy_string_to_buffer_loop:
    ld (de), a
    inc de
    djnz _editor_copy_string_to_buffer_loop
    ; Force a newline at the end
_editor_copy_string_to_buffer_ret:
    ld a, '\n'
    ld (de), a
    pop bc
    inc bc
    ret


    ; Redraw the current line. If the line was horizontally scrolled, it will
    ; start back from its first character.
    ; The cursor will be positioned at the beginning of the following line.
    ; Parameters:
    ;   None
    ; Returns:
    ;   None;
    ; Alters:
    ;   A, BC, DE, HL
_editor_view_restore_current_line:
    ld a, 1
    call _buffer_previous_ath_line_address
    call _editor_copy_string_to_buffer
    push bc
_editor_view_restore_line_from_buffer:
    ; Re-position the cursors to the beginning of the line
    ld hl, 0
    ld (_file_cursor_x), hl
    xor a
    ld (_cursor_x), a
    call _editor_view_restore_cursor
    ; Print the current buffer
    pop bc
    ld de, _editor_view_buffer
    S_WRITE1(DEV_STDOUT)
    ret

    ; Same as above but for the next line, which is bigger than the
    ; screen width for sure.
    ; Parameters:
    ;   None
    ; Returns:
    ;   None
    ; Alters:
    ;   A, BC, DE, HL
_editor_view_restore_next_line:
    call model_next_line_address
    ld bc, (_editor_screen_width)
    push bc
    ld de, _editor_view_buffer
    ldir
    ld a, '\n'
    ld (de), a
    jp _editor_view_restore_line_from_buffer


    ; Move cursor to the next line
    ; Parameters:
    ;   BC - Length of the new line
    ;   HL - Newline characters
    ; Parameters must be popped out of the stack!
    PUBLIC _editor_view_goto_newline
_editor_view_goto_newline:
    ; We have to increment Y, if it is at the bottom of the page, we must
    ; manually scroll the screen by redrawing all the previous lines
    call _editor_cursor_bottom_line
    jp z, _editor_cursor_move_lines_up
    push hl
    ; Optimize a bit by restoring the current line if BC is not 0 or the current
    ; line is scrolled
    call _editor_view_restore_current_line_if_needed
    ; Get the remaining number of lines, so that we can update them
    call model_get_remaining_lines
    ; The total includes the newly created line.
    ld b, h
    ld c, l
    ; Get the remaining number of lines that we can show on screen
    ld hl, _cursor_y
    inc (hl)
    ld a, (_editor_screen_height)
    sub EDITOR_FOOTER_HEIGHT
    sub (hl)
    ; Store the result in HL
    ld h, 0
    ld l, a
    call editor_min_hl_bc
    ; The new line address was given to us as a parameter
    pop hl
    ; Move the number of lines to update in B
    ld b, c
    call _editor_view_print_b_lines
    jp _editor_view_goto_newline_size_0

    ; Checks if the current line needs to be restored, adn restores it
_editor_view_restore_current_line_if_needed:
    call editor_view_long_line
    jp nz, _editor_view_restore_current_line
    ld a, b
    or c
    jp nz, _editor_view_restore_current_line
    ; Position the cursor at the beginning of the next line
    ld a, (_cursor_y)
    inc a
    ld e, a
    ld d, 0
    jp _editor_view_restore_cursor_no_load


    ; Jump to here if the cursor is at the bottom of the screen and we need to
    ; go to the next line. In order word, we will scroll down all the lines
    ; (by moving them up once...)
_editor_cursor_move_lines_up:
    ld a, (_editor_screen_height)
    sub EDITOR_FOOTER_HEIGHT + 1
    ; Save the new line size
    push bc
    call _editor_view_scroll_down
    pop bc
    ; As scrolling printed all the lines, including the new one, the only thing we
    ; have to do now is clean BC characters on screen. Because these chars used to
    ; be at the end of the previous line.
    ld a, b
    or c
    ; If the size is 0, nothing to clean, simply reposition the cursor
    jr z, _editor_view_goto_newline_size_0
    ; Calculate the minimum between the screen width and the number of characters to
    ; clean on screen
    push bc
    call _editor_view_erase_buffer
    pop hl
    call editor_min_hl_bc
    ld de, _editor_view_buffer
    S_WRITE1(DEV_STDOUT)
    ; The position is 0 for sure because we just jumped to a new line.
    ; Store 0 in X and absolute X.
_editor_view_goto_newline_size_0:
    ld hl, 0
    ld (_file_cursor_x), hl
    ld a, l
    ld (_cursor_x), a
    jp _editor_view_restore_cursor


    ; Reposition the cursor sent by the model if necessary and print the current
    ; line at the current cursor location.
    ; Parameters:
    ;   H - Former X coordinate
    ;   BC - New X position given by the model
    ; Returns:
    ;   BC - New coordinates
_editor_view_reposition_cursor:
    ld a, h
    ld l, a
    ld h, 0
    or a
    sbc hl, bc
    ; If no carry, the buffer is only printed on screen, no need to modify it either
    ret nc
    ld h, 0
    ld l, a
    push hl
    ; The screen is actually already showing the beginning of the line, so there
    ; is no need to re-print it (this routine was entered because we don't need
    ; to scroll)
    call model_move_cursor_left_to
    pop bc
    ret


    ; Move the cursor to the previous line
    ; Parameters:
    ;   BC - New cursor position after moving to the previous line
    PUBLIC _editor_view_cursor_prev_line
_editor_view_cursor_prev_line:
    push bc
    ld a, (_cursor_y)
    dec a
    jp z, _editor_view_prev_line_scroll
    ; No need to scroll here, first, check if the current line is a long line (and the
    ; cursor is beyond the limit)
    call editor_view_long_line
    ; Save the current X value
    ld a, (_cursor_x)
    push af
    ; If it is, we need to restore current line (which is now considered next by the
    ; model as the cursor was repositioned)
    call nz, _editor_view_restore_next_line
    ; Current line was restored, save the new coordinate
    ld hl, _cursor_y
    dec (hl)
_editor_view_line_changed_restore:
    ; New cursor position must not be scrolled, so we have to reposition it.
    pop hl
    pop bc
    ; This label can be used to reposition and restore the model cursor to a value
    ; that won't require horizontal scrolling
_editor_view_reposition_model_and_restore:
    call _editor_view_reposition_cursor
    ; Returns the new X coordinate in BC
    ld (_file_cursor_x), bc
    ld a, c
    ld (_cursor_x), a
    jp _editor_view_restore_cursor

_editor_view_next_line_scroll:
    ld a, (_editor_screen_height)
    sub EDITOR_FOOTER_HEIGHT + 1
    call _editor_view_scroll_down
    jp _editor_view_reposition_after_scroll

_editor_view_prev_line_scroll:
    call _editor_view_scroll_up
    ; Fall-through

_editor_view_reposition_after_scroll:
    pop bc
    ; All the lines have been scrolled, and the current one has been updated, we simply
    ; need to update the cursor now
    ld a, (_cursor_x)
    ld h, a
    jp _editor_view_reposition_model_and_restore


    ; Move the cursor to the next line
    ; Parameters:
    ;   BC - New cursor position after moving to the next line
    PUBLIC _editor_view_cursor_next_line
_editor_view_cursor_next_line:
    push bc
    call _editor_cursor_bottom_line
    jp z, _editor_view_next_line_scroll
    ; No scroll needed but we may need to restore the current line if
    ; it was horizontally scrolled.
    call editor_view_long_line
    ; Save the current X value
    ld a, (_cursor_x)
    push af
    ; If it is, we need to restore current line (which is now considered
    ; previous by the model as the cursor was repositioned)
    call nz, _editor_view_restore_current_line
    ; Current line was restored, save the new coordinate
    ld hl, _cursor_y
    inc (hl)
    jp _editor_view_line_changed_restore


    ; Prepare the buffer and the watermark counter before a scroll loop
    ; Alters:
    ;   HL, DE, A
_editor_view_scroll_init:
    ; Add \n at the end of the buffer FOR THE CURRENT SCREEN WIDTH
    ld a, '\n'
    ld hl, _editor_view_buffer
    ld de, (_editor_screen_width)
    add hl, de
    ld (hl), a
    ; Set the previous line length to the maximum so that it's cleaned
    ; by the first line to print
    ld a, e
    ld (_editor_view_8bit_counter), a
    ; Write the code that "returns" the length of the line to override in A
    ; For scrolling down: ld a, (_editor_view_8bit_counter)
    ld a, 0x3a                          ; ld a, (nnnn)
    ld hl, _editor_view_8bit_counter    ; nnnn
    ld (_editor_view_copy_and_fill_modify_me), a
    ld (_editor_view_copy_and_fill_modify_me + 1), hl
    ret


    ; Calculate the length of the string present in HL
    ; This routine must NOT modify any register (except A of course)
    ; Parameters:
    ;   HL - Address of the line to erase
    ; Returns:
    ;   A - Length of the line to erase
_editor_get_line_to_erase_length:
    push hl
    push bc
    ld bc, (_editor_screen_width)
    push bc
    ld a, '\n'
    cpir
    pop hl
    or a
    sbc hl, bc
    ; The result will be 8-bit because the screen width is 8-bit
    ld a, l
    pop bc
    pop hl
    ret


    ; Parameters:
    ;   B - Number of lines to print
    ;   HL - Address of the buffer containing the lines
_editor_view_print_b_lines:
    push hl
    call _editor_view_scroll_init
    pop hl
    jp _editor_view_print_n_lines


    ; Scroll the lines up on screen.
    ; Parameters:
    ;   BC - New position of the cursor after moving it to the previous line
    ; Returns:
    ;   -
_editor_view_scroll_up:
    ; Set the cursor to the top of the view and start drawing
    ld de, 1   ; Set the cursor to (0,1)
    call _editor_view_restore_cursor_no_load
    ; Set the watermark to the minimum
    call _editor_view_scroll_init
    ; Print current line WITH newline at the end
    ld a, 1
    call _editor_view_print_current_line
    call model_next_line_address
    ; Number of lines to restore
    ld a, (_editor_screen_height)
    sub EDITOR_FOOTER_HEIGHT + 2
    ld b, a
_editor_view_print_n_lines:
    dec b
    inc b
    ret z
    ; Modify the code in the routine _editor_view_copy_and_fill to calculate the length of the line
    ; to override
    ; New code: call _editor_get_line_to_erase_length
    ld a, 0xcd
    ld de, _editor_get_line_to_erase_length
    ld (_editor_view_copy_and_fill_modify_me), a
    ld (_editor_view_copy_and_fill_modify_me + 1), de
_editor_view_scroll_up_loop:
    push bc
    ; Fill the temporary buffer with the current line data
    call _editor_view_copy_and_fill
    ; Print the line on screen
    push hl
    S_WRITE1(DEV_STDOUT)
    pop hl
    pop bc
    djnz _editor_view_scroll_up_loop
    ret


    ; Scroll the lines on screen, optimized by only writing enough characters
    ; to hide previous line content. Speed now depends on the amount of characters
    ; on screen.
    ; For lines that have around 1-4 characters, this routine is now 8x faster, for a
    ; whole screen refresh.
    ; Parameters:
    ;   A - Number of lines to scroll
_editor_view_scroll_down:
    ; Get the A-th previous line of text. For example we have 36 lines to show,
    ; 36th is the first one (top), because 0 is the new empty one. Ignore the
    ; top most line, so decrement A.
    dec a
    ld d, a ; D is not altered
    call _buffer_previous_ath_line_address
    push hl
    push de
    ; Set the cursor to the top of the view and start drawing
    ld de, 1   ; Set the cursor to (0,1)
    call _editor_view_restore_cursor_no_load
    call _editor_view_scroll_init
    ; Store the number of iterations in BC
    pop bc
    pop hl
_editor_view_scroll_down_loop:
    push bc
    ; Fill the temporary buffer with the current line data
    call _editor_view_copy_and_fill
    ; Print the line on screen
    push hl
    S_WRITE1(DEV_STDOUT)
    pop hl
    pop bc
    djnz _editor_view_scroll_down_loop
    ; Print the current line without a \n, to prevent the cursor from going to the next line
    xor a
    ; Print the model current line
    ; Parameters:
    ;   A - 1 if print a newline at the end of the current line, 0 else
_editor_view_print_current_line:
    push af
    ; Write the last line, which is the current line
    ld de, _editor_view_buffer
    ; B will be 0, so the routine will return B = 0
    ld bc, (_editor_screen_width)
    call model_get_current_line
    ld a, c
    call _editor_view_copy_and_fill_size_in_a
    ; Decrement C if we must not include/print the final \n
    pop af
    or a
    ; A != 0 <=> Do not decrement C
    jr nz, _editor_view_print_current_line_with_newline
    dec c
_editor_view_print_current_line_with_newline:
    S_WRITE1(DEV_STDOUT)
    ret


    ; Parameters:
    ;   HL - Address of the current line to print
    ; Returns:
    ;   DE - Address of the buffer to print
    ;   HL - Address of the next line to print
    ; Alters:
    ;   A, BC, DE, HL
_editor_view_copy_and_fill:
    ld de, _editor_view_buffer
    ld a, '\n'
    ; Maximum size to write is the size of the screen
    ld bc, (_editor_screen_width)
    push bc
_editor_view_copy_and_fill_loop:
    cp (hl)
    jr z, _editor_view_copy_and_fill_end
    ldi
    jp pe, _editor_view_copy_and_fill_loop
    ; The given line from the model has a bigger size than the screen size
    ; Look for its ending \n, to return next line address
    cpir
    ; Add a newline at the end of DE
    ld (de), a
    ; Pop the size of the current line in BC
    pop bc
    ; Save the current buffer size
    ld a, c
    ; Increment C after setting A as the newline doesn't count as part of the current line length
    inc c
    jp _editor_view_copy_and_fill_ret
_editor_view_copy_and_fill_end:
    inc hl  ; Make HL point to the character after the newline
    ; Add a new line at the end of DE
    ld (de), a
    ; Calculate the current size
    ld a, c
    pop bc
    ld b, a
    ld a, c
    sub b
_editor_view_copy_and_fill_size_in_a:
    ; Check if the previous buffer was bigger than the current, if so,
    ; fill the difference with spaces
    ld b, a
    ; Code that will be modified at runtime, upon execution, it stores the length
    ; of the size to overwrite in A
    ; For scrolling down: ld a, (_editor_view_8bit_counter)
    ; For scrolling up: call _editor_get_line_to_erase_length
_editor_view_copy_and_fill_modify_me:
    nop
    nop
    nop
    ld c, a
    ; Calculate current_size - previous_size
    ld a, b
    sub c
    ; If there is no carry, the previous size was smaller, no need to fill
    ; the buffer with spaces but we have to update the size.
    jp nc, _editor_view_copy_and_fill_smaller
    ; Fill the buffer with |A - C| spaces, current length is still in B.
    neg
    ld c, b ; Current length in C
    ld b, a ; Number of spaces to write
    ; Save the current length in the global variable
    ld a, c
    ld (_editor_view_8bit_counter), a
    ; At the end, return BC = current length + number of spaces written
    add b
    push af
    ex de, hl
    ld a, ' '
_editor_view_copy_and_fill_end_loop:
    ld (hl), a
    inc hl
    djnz _editor_view_copy_and_fill_end_loop
    ld (hl), '\n'
    ex de, hl
    ; Return the number of characters to write on screen
    pop bc
    ld c, b
    ld b, 0
    ; Increment (B)C because of the final \n
    inc c
    ld de, _editor_view_buffer
    ret
_editor_view_copy_and_fill_smaller:
    ; The current size, in B, is bigger or equal to the previous line size.
    ; We need to print B characters.
    ld c, b
    ld b, 0
    ld a, c
    ; Increment (B)C because a '\n' was added
    inc c
_editor_view_copy_and_fill_ret:
    ld (_editor_view_8bit_counter), a
    ld de, _editor_view_buffer
    ret


    SECTION DATA

    ; The following represent the cursor on screen, it is synchronized with the
    ; actual driver's cursor
_cursor_y:  DEFS 1
_cursor_x:  DEFS 1

    ; This cursor represents the absolute value of the cursor on the line.
    ; If a line is longer than the screen width, this cursor will still be incremented
    ; and monotonic
_file_cursor_x: DEFS 2

    ; Using a 16-bit value will simply the code
_editor_screen_width: DEFS 2
_editor_screen_height: DEFS 1
    ; Temporary buffer used to store data to print
    PUBLIC _editor_view_buffer
_editor_view_buffer:
    DEFS 80
    DEFM '\n'
_editor_view_buffer_end:
    ; 8-bit iterator for loops
_editor_view_8bit_counter: DEFS 1
