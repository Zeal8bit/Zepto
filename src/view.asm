; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    INCLUDE "zos_video.asm"

    DEFC EDITOR_CHARS_COLOR      = (TEXT_COLOR_BLACK << 8) | TEXT_COLOR_WHITE
    DEFC EDITOR_BACKGROUND_COLOR = (TEXT_COLOR_WHITE << 8) | TEXT_COLOR_BLACK
    DEFC EDITOR_FOOTER_HEIGHT = 3

    EXTERN _buffer_line_num
    EXTERN _buffer_previous_ath_line_address
    EXTERN model_next_line_address
    EXTERN model_get_bc_left_chars
    EXTERN model_get_bc_right_chars
    EXTERN _model_divide_hl_c
    EXTERN _buffer_memory_end
    EXTERN _buffer_col_num
    EXTERN _buffer_cur_length
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
    ; The model's absolute column is the only absolute cursor. The view only
    ; stores the column shown at screen x = 0.
    ld hl, 0
    ld (_view_window_start), hl
    xor a
    ld (_cursor_x), a
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

    ;=========================================================;
    ;===================  LINE RENDERING  ====================;
    ;=========================================================;

    DEFC VIEW_WIDTH = 80
    DEFC VIEW_HEIGHT = 40
    DEFC VIEW_TEXT_BOTTOM = VIEW_HEIGHT - EDITOR_FOOTER_HEIGHT - 1
    DEFC VIEW_SCROLL_STEP = VIEW_WIDTH - 1

    ; Set _view_window_start and _cursor_x for the absolute column given in HL.
    ; For columns beyond the first screen, the window starts on a multiple of
    ; (width - 1), which keeps one character of left context when scrolling.
    ; Parameters:
    ;   HL - Absolute column
    ; Returns:
    ;   None
    ; Alters:
    ;   A, BC, HL
_view_set_window_for_col:
    ld a, h
    or l
    jr z, _view_set_window_zero
    push hl
    dec hl
    ld c, VIEW_SCROLL_STEP
    call _model_divide_hl_c
    ; HL = quotient, A = remainder
    inc a                           ; x = remainder + 1
    ld b, a
    ld (_cursor_x), a
    pop hl                          ; HL = absolute column
    ld c, b
    ld b, 0
    or a
    sbc hl, bc                      ; start = column - x
    ld (_view_window_start), hl
    ret

_view_set_window_zero:
    xor a
    ld (_cursor_x), a
    ld hl, 0
    ld (_view_window_start), hl
    ret


    ; Same as above, taking the column from the model.
_view_set_window_from_model:
    ld hl, (_buffer_col_num)
    jp _view_set_window_for_col


    ; Render the current line window at _cursor_y. _cursor_x characters are
    ; taken from the left of the gap, the rest from the right side, and the
    ; row is padded to the screen width. The hardware cursor is left at the
    ; beginning of the next row, so the caller can print the following rows.
    ; Parameters:
    ;   None
    ; Returns:
    ;   None
    ; Alters:
    ;   A, BC, DE, HL
_view_render_current_line:
    ld de, _editor_view_buffer
    ld a, (_cursor_x)
    or a
    jr z, _view_render_no_left
    ld c, a
    ld b, 0
    call model_get_bc_left_chars
    ldir
_view_render_no_left:
    ; Number of characters that still fit on the screen line
    ld a, VIEW_WIDTH
    ld hl, _cursor_x
    sub (hl)
    ld c, a
    ld b, 0
    call model_get_bc_right_chars
    ld a, b
    or c
    jr z, _view_render_no_right
    push bc
    ldir
    pop bc
    jr _view_render_pad
_view_render_no_right:
    ld bc, 0
_view_render_pad:
    ; Pad the rest of the window with spaces
    ld a, VIEW_WIDTH
    ld hl, _cursor_x
    sub (hl)
    sub c
    jr z, _view_render_ready
    ld c, a
    ld b, 0
_view_render_pad_loop:
    ld a, ' '
    ld (de), a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, _view_render_pad_loop
_view_render_ready:
    ld a, '\n'
    ld (_editor_view_buffer + VIEW_WIDTH), a
    ld a, (_cursor_y)
    ld e, a
    ld d, 0
    call _editor_view_restore_cursor_no_load
    ld de, _editor_view_buffer
    ld bc, VIEW_WIDTH + 1
    S_WRITE1(DEV_STDOUT)
    ret


    ; Print the line pointed by HL at the current cursor position, padded to
    ; the screen width, and make HL point to the next line. The line must not
    ; cross the gap: only lines entirely on the left or on the right side can
    ; be given.
    ; Parameters:
    ;   HL - Address of the line to print
    ; Returns:
    ;   HL - Address of the next line
    ; Alters:
    ;   A, BC, DE, HL
_editor_view_print_line_padded:
    ld de, _editor_view_buffer
    ld bc, VIEW_WIDTH
_editor_view_print_line_loop:
    ld a, (hl)
    cp '\n'
    jr z, _editor_view_print_line_end
    ld (de), a
    inc de
    inc hl
    dec bc
    ld a, b
    or c
    jr nz, _editor_view_print_line_loop
    ; The line is longer than the screen, skip its remaining characters
_editor_view_print_line_scan:
    ld a, (hl)
    inc hl
    cp '\n'
    jr nz, _editor_view_print_line_scan
    jr _editor_view_print_line_pad
_editor_view_print_line_end:
    inc hl
_editor_view_print_line_pad:
    ld a, b
    or c
    jr z, _editor_view_print_line_write
_editor_view_print_line_pad_loop:
    ld a, ' '
    ld (de), a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, _editor_view_print_line_pad_loop
_editor_view_print_line_write:
    ld a, '\n'
    ld (_editor_view_buffer + VIEW_WIDTH), a
    push hl
    ld de, _editor_view_buffer
    ld bc, VIEW_WIDTH + 1
    S_WRITE1(DEV_STDOUT)
    pop hl
    ret


    ; Print a blank row at the current cursor position.
_view_print_blank_line:
    ld hl, _editor_view_buffer
    ld bc, VIEW_WIDTH
_view_print_blank_loop:
    ld (hl), ' '
    inc hl
    dec bc
    ld a, b
    or c
    jr nz, _view_print_blank_loop
    ld a, '\n'
    ld (_editor_view_buffer + VIEW_WIDTH), a
    ld de, _editor_view_buffer
    ld bc, VIEW_WIDTH + 1
    S_WRITE1(DEV_STDOUT)
    ret


    ; Print the line pointed by HL, or a blank row when HL is past the end of
    ; the buffer.
    ; Parameters:
    ;   HL - Address of the line to print
    ; Returns:
    ;   HL - Address of the next line (unchanged when blank)
    _view_print_line_or_blank:
    push hl
    ld de, (_buffer_memory_end)
    inc de
    or a
    sbc hl, de
    pop hl
    jr nc, _view_print_line_blank
    jp _editor_view_print_line_padded
_view_print_line_blank:
    ; Preserve HL: the blank printer uses it, and the caller must keep the
    ; "past the end" address so that the following rows are blank too.
    push hl
    call _view_print_blank_line
    pop hl
    ret


    ; Print the line pointed by HL at the given row, padded to the screen
    ; width.
    ; Parameters:
    ;   A - Row (1-based)
    ;   HL - Address of the line
    _view_print_line_at_row:
    push hl
    ld e, a
    ld d, 0
    call _editor_view_restore_cursor_no_load
    pop hl
    jp _view_print_line_or_blank


    ; Return the start address of the current line. The current line prefix is
    ; the last _buffer_col_num characters on the left side of the gap, so its
    ; start is simply left_addr - column.
    ; Parameters:
    ;   None
    ; Returns:
    ;   HL - Start of the current line
    _view_get_line_start:
    ld bc, (_buffer_col_num)
    call model_get_bc_left_chars
    ret


    ; Walk back a given number of lines from a line start address.
    ; Parameters:
    ;   HL - Start of the current line
    ;    A - Number of previous lines to go back
    ; Returns:
    ;   HL - Start of the A-th previous line
    _view_previous_lines:
    or a
    ret z
    ld b, a
_view_previous_lines_loop:
    ; HL points to the start of the line. The two bytes before it are the
    ; previous line's terminator and (possibly) its last character.
    dec hl
    dec hl
_view_previous_lines_scan:
    ld a, (hl)
    cp '\n'
    jr z, _view_previous_lines_found
    dec hl
    jr _view_previous_lines_scan
_view_previous_lines_found:
    inc hl
    dec b
    jr nz, _view_previous_lines_loop
    ret


    ; Redraw the whole text area. The current line is at _cursor_y and the
    ; model column is the cursor position. Lines above and below the current
    ; one are printed from their beginning.
    _view_full_redraw:
    call _view_set_window_from_model
    ; For a non-empty current line, the model's A-th previous line helper is
    ; the cheapest way to get the top line. An empty current line starts on a
    ; '\n', where that helper is off by one, so use the line-start walker.
    ld hl, (_buffer_cur_length)
    ld a, h
    or l
    jr z, _view_full_empty_line
    ld a, (_cursor_y)
    dec a
    call _buffer_previous_ath_line_address
    jr _view_full_top_ready

_view_full_empty_line:
    call _view_get_line_start
    push hl
    ld a, (_cursor_y)
    dec a
    ld c, a
    ld hl, (_buffer_line_num)
    ld b, 0
    or a
    sbc hl, bc
    jr nc, _view_full_empty_ready
    ld a, (_buffer_line_num)
    ld c, a
_view_full_empty_ready:
    pop hl
    ld a, c
    call _view_previous_lines

_view_full_top_ready:
    ; Position the hardware cursor at the top of the text area. The IOCTL may
    ; clobber HL, so preserve the top line address across it.
    push hl
    ld de, 1
    call _editor_view_restore_cursor_no_load
    pop hl
    ld b, 1
_view_full_loop:
    ld a, (_cursor_y)
    cp b
    jr z, _view_full_cursor_row
    push bc
    call _view_print_line_or_blank
    pop bc
    jr _view_full_next
_view_full_cursor_row:
    push bc
    call _view_render_current_line
    call model_next_line_address
    pop bc
_view_full_next:
    inc b
    ld a, b
    cp VIEW_TEXT_BOTTOM + 1
    jr nz, _view_full_loop
    jp _editor_view_restore_cursor


    ; Recompute the window for the current model column, redraw the current
    ; line and restore the cursor.
    _view_after_edit:
    call _view_set_window_from_model
    call _view_render_current_line
    jp _editor_view_restore_cursor


    ; Public view entry points called by the model after a buffer edit. The
    ; whole line is redrawn; the model has already updated its length/column.
    PUBLIC _editor_view_insert_at_cursor
_editor_view_insert_at_cursor:
    PUBLIC _editor_view_insert_string_at_cursor
_editor_view_insert_string_at_cursor:
    PUBLIC _editor_view_insert_string_before_cursor
_editor_view_insert_string_before_cursor:
    PUBLIC _editor_view_remove_at_cursor
_editor_view_remove_at_cursor:
    jp _view_after_edit


    ; Move the cursor left. The model already updated the column.
    PUBLIC _editor_view_left_cursor
_editor_view_left_cursor:
    ld a, (_cursor_x)
    or a
    jr z, _view_move_recompute
    dec a
    jr z, _view_move_recompute
    ld (_cursor_x), a
    jp _editor_view_restore_cursor


    ; Move the cursor right. The model already updated the column.
    PUBLIC _editor_view_right_cursor
_editor_view_right_cursor:
    ld a, (_cursor_x)
    inc a
    cp VIEW_WIDTH
    jr nc, _view_move_recompute
    ld (_cursor_x), a
    jp _editor_view_restore_cursor

_view_move_recompute:
    call _view_set_window_from_model
    call _view_render_current_line
    jp _editor_view_restore_cursor


    ; Redraw the current line after Home/End and restore the cursor.
    PUBLIC _editor_view_redraw_current_line_and_restore
_editor_view_redraw_current_line_and_restore:
    call _view_set_window_from_model
    call _view_render_current_line
    jp _editor_view_restore_cursor


    ; Previous line joined into the current one: the merged line moves one row
    ; up (or stays on the first row).
    PUBLIC _editor_view_join_previous_line
_editor_view_join_previous_line:
    ld a, (_cursor_y)
    cp 1
    jr z, _view_join_previous_row
    dec a
_view_join_previous_row:
    ld (_cursor_y), a
    jp _view_full_redraw


    ; Next line joined into the current one: the current row is unchanged.
    PUBLIC _editor_view_join_next_line
_editor_view_join_next_line:
    jp _view_full_redraw


    ; A new line was inserted: move to the next row, scrolling if needed.
    PUBLIC _editor_view_goto_newline
_editor_view_goto_newline:
    ld a, (_cursor_y)
    cp VIEW_TEXT_BOTTOM
    jr z, _view_newline_scroll
    inc a
    ld (_cursor_y), a
_view_newline_scroll:
    jp _view_full_redraw


    ; Move the cursor to the previous line. The model already updated the
    ; line/column. Only the old line and the new current line are redrawn,
    ; unless the viewport has to scroll.
    PUBLIC _editor_view_cursor_prev_line
_editor_view_cursor_prev_line:
    ld a, (_cursor_y)
    cp 1
    jr z, _view_prev_scroll
    ld hl, (_view_window_start)
    push hl
    ld a, (_cursor_y)
    dec a
    ld (_cursor_y), a
    call _view_set_window_from_model
    pop de
    ld a, d
    or e
    jr z, _view_prev_render
    ; The old line is now the next line. Redraw it from its beginning.
    call model_next_line_address
    ld a, (_cursor_y)
    inc a
    call _view_print_line_at_row
_view_prev_render:
    call _view_render_current_line
    jp _editor_view_restore_cursor
_view_prev_scroll:
    ld a, 1
    ld (_cursor_y), a
    jp _view_full_redraw


    ; Move the cursor to the next line. The model already updated the
    ; line/column. Only the old line and the new current line are redrawn,
    ; unless the viewport has to scroll.
    PUBLIC _editor_view_cursor_next_line
_editor_view_cursor_next_line:
    ld a, (_cursor_y)
    cp VIEW_TEXT_BOTTOM
    jr z, _view_next_scroll
    ld hl, (_view_window_start)
    push hl
    ld a, (_cursor_y)
    inc a
    ld (_cursor_y), a
    call _view_set_window_from_model
    pop de
    ld a, d
    or e
    jr z, _view_next_render
    ; The old line is now the previous line. Redraw it from its beginning.
    call _view_get_line_start
    ld a, 1
    call _view_previous_lines
    ld a, (_cursor_y)
    dec a
    call _view_print_line_at_row
_view_next_render:
    call _view_render_current_line
    jp _editor_view_restore_cursor
_view_next_scroll:
    ld a, VIEW_TEXT_BOTTOM
    ld (_cursor_y), a
    jp _view_full_redraw


    SECTION DATA

    ; The following represents the cursor on screen, it is synchronized with the
    ; actual driver's cursor
_cursor_y:  DEFS 1
_cursor_x:  DEFS 1

    ; Absolute column displayed at screen x = 0
_view_window_start: DEFS 2

    ; Using a 16-bit value will simply the code
_editor_screen_width: DEFS 2
_editor_screen_height: DEFS 1
    ; Temporary buffer used to store data to print
    PUBLIC _editor_view_buffer
_editor_view_buffer:
    DEFS VIEW_WIDTH
    DEFM '\n'
