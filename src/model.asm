; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"

    DEFC EDITOR_MAX_COL = (80 - 1)

    EXTERN _editor_view_insert_string_at_cursor
    EXTERN _editor_view_insert_at_cursor
    EXTERN _editor_view_insert_string_before_cursor
    EXTERN _editor_view_remove_at_cursor
    EXTERN _editor_view_left_cursor
    EXTERN _editor_view_right_cursor
    EXTERN _editor_view_clean_string_at_cursor
    EXTERN _editor_view_goto_newline
    EXTERN _editor_view_cursor_prev_line
    EXTERN _editor_view_cursor_prev_line
    EXTERN _editor_view_8bit_var
    EXTERN _editor_view_cursor_next_line
    EXTERN _editor_view_init_new_line
    EXTERN _editor_view_restore_cursor
    EXTERN editor_show_notification
    EXTERN _start_buffer
    EXTERN strlen
    ; Use this buffer as temporary one only!
    EXTERN _editor_view_buffer
    DEFC TEMPORARY_BUFFER = _editor_view_buffer

    ;=========================================================;
    ;=====================  MODEL ============================;
    ;=========================================================;

    SECTION TEXT


    ; Text buffer address (RAM bank)
    DEFC BUFFER_START_ADDRESS = _start_buffer
    DEFC BUFFER_MAX_SIZE = 32*1024
    DEFC BUFFER_END_ADDRESS = _start_buffer + BUFFER_MAX_SIZE - 1
    DEFC BUFFER_MAX_LINE_LENGTH = 0xFFFF

    DEFC EDITOR_MAX_LINE = (40 - 1) - 4

    ; Parameters:
    ;   DE - File name to open
    PUBLIC _editor_model_init
_editor_model_init:
    ld hl, _start_buffer
    ld (_buffer_left_addr), hl
    ld hl, BUFFER_END_ADDRESS
    ld (_buffer_right_addr), hl
    ; To simplify the calculations for the last line, add a final \n
    inc hl
    ld (hl), '\n'
    ld hl, 1
    ld (_buffer_line_count), hl
    ld hl, 0
    ld (_buffer_line_num), hl
    ld (_buffer_col_num), hl
    ; Check if a file was given
    ld a, d
    or e
    ret z
    ; TODO: Check that the filename is valid?
    ; A file was given, open it, save the full path
    ld (_model_full_path), de
    ld b, d
    ld c, e
    ld h, O_RDONLY
    OPEN()
    ; If an error occurred, return
    or a
    jp m, _editor_model_open_error
    ; Save file descriptor in a static variable
    ld (_model_file_dev), a
    ; Check if the file is too big, use stat to get file info
    ld de, TEMPORARY_BUFFER
    ld h, a ; opened device descriptor
    DSTAT()
    or a
    jp nz, _editor_model_init_error
    ; Make sure the file size doesn't go above the limit
    ex de, hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    ; DE contains the size of the file
    ld hl, BUFFER_MAX_SIZE
    sbc hl, de  ; C is 0 already
    jp c, _editor_model_init_error
    ; TODO: check if the entry is a directory (once d_flags is part of stat structure)
    ; We can start reading from the file, the buffer of destination is spread across
    ; three virtual pages, so we will perform `read` three times if necessary.
    ld de, BUFFER_START_ADDRESS
    ; First bank finishes at 0x8000
    ld bc, 0x8000 - BUFFER_START_ADDRESS
    push bc
    call _editor_model_read_bc
    ; We can now close the file
    ld a, (_model_file_dev)
    ld h, a
    CLOSE()
    ; DE contains the remaining size in the buffer, so the size read from the
    ; file is BC - DE
    or a    ; clear carry flag
    pop hl
    sbc hl, de
    ; If BC is 0, we have nothing to read more from the file, else, we have to
    ; continue reading, inside the rest of the buffer.
    ld a, b
    or c
    ; TODO: Support copying to the next buffers
    jp nz, _editor_model_open_error
    ; Copy the file content to the end of the buffer
    ld de, BUFFER_END_ADDRESS
    ; Copy the file size (number of bytes to copy) in BC
    ld b, h
    ld c, l
    ; Calculate the address of the last byte of the loaded file
    ld hl, BUFFER_START_ADDRESS
    add hl, bc
    ; HL points to the next free character, decrement it to point to the last char of the file
    dec hl
    lddr
    ; Copy the characters to the view, DE contains the address of the first char (-1)
    ; Set the right cursor to this value
    ld (_buffer_right_addr), de
    inc de
    ex de, hl
    ; Manually handle the first line
    call _model_line_show_lines
    ld (_buffer_cur_length), bc
_model_lines_show_loop:
    ; Put next line address in DE
    ex de, hl
    ; We can continue if the address of the next line is not the end
    ld hl, BUFFER_END_ADDRESS - 1    ; - 1 because we want C to be set if equal
    sbc hl, de
    jr c, _model_lines_show_loop_end
    ; Do not print the line to the screen if we reached the end of the screen
    ; (we are simply counting the total number of lines)
    ; Increment the total number of lines
    ld hl, (_buffer_line_count)
    inc hl
    ld (_buffer_line_count), hl
    ld a, h
    or a
    jr nz, _model_lines_show_loop
    or l
    cp EDITOR_MAX_LINE + 1
    ; Put back the next line address in HL before (potentially) jumping
    ex de, hl
    jr nc, _model_lines_show_loop
    call _model_line_show_lines
    jp _model_lines_show_loop
_model_lines_show_loop_end:
    jp _editor_view_restore_cursor


    ; Parameter:
    ;   HL - Address of the line to show
    ;    B - Line iteration
    ; Returns:
    ;   HL - Address of the next line
    ;    C - Length of the line
_model_line_show_lines:
    push hl
    ; Calculate the length of the first line
    call _editor_model_line_length
    ; Get the original line back
    ex (sp), hl
    ; Save the length on the stack
    push bc
    call _editor_view_init_new_line
    pop bc
    pop hl
    ret


    ; Parameters:
    ;   HL - Address of the line
    ; Returns:
    ;   BC - Length of the line
    ;   HL - Address of the next line
_editor_model_line_length:
    ld bc, 0
    ld a, '\n'
    push bc
    cpir
    ; Put next line address in DE
    ex de, hl
    pop hl
    xor a
    sbc hl, bc
    ; Do not count the \n in the line length
    dec hl
    ld b, h
    ld c, l
    ex de, hl
    ret


_editor_model_read_bc:
    ; Put the opened file dev in H
    ld a, (_model_file_dev)
    ld h, a
    ; Save the maximum size to read on the stack
    push bc
    READ()
    ; BC contains the number of bytes read from the file
    pop hl
    or a
    jp nz, _editor_model_init_error
    ; Carry flag is 0 because of `or a`
    sbc hl, bc
    ; Put the result in DE
    ex de, hl
    ; If the result is 0, we have to read in the next buffer
    ret z
    ; If BC is 0, we finished reading the file
    ld a, b
    or c
    ret z
    ; Else, continue reading the file, maximum remaining size is in DE,
    ; HL is the destination of the buffer.
    add hl, bc
    ex de, hl
    ld b, h
    ld c, l
    jp _editor_model_read_bc


_editor_model_open_error:
    ; If the error is "ERR_NO_SUCH_ENTRY", it's not really an error as we will
    ; create this file, so don't exit is that case
    cp -ERR_NO_SUCH_ENTRY
    ret z
_editor_model_init_error:
    S_WRITE3(DEV_STDOUT, _editor_model_error_str, _editor_model_error_str_end - _editor_model_error_str)
    EXIT()

_editor_model_error_str:
    DEFM "ERROR OPENING THE FILE\n", 0
_editor_model_error_str_end:

    ; Get the size of the line after the cursor
    ; Parameters:
    ;   None
    ; Returns:
    ;   HL - Number of characters after the cursor on the current line
    ; Alters:
    ;   HL, BC
_buffer_size_right_line:
    ld hl, (_buffer_cur_length)
    ld bc, (_buffer_col_num)
    sbc hl, bc
    ret


    ; Generic function to insert a character in the buffer, at the left address
    ; The only check performed here is whether the file is full or not.
    ; No check is performed for the current line length, it shall be done by
    ; the caller. This routine also updates the _file_size global variable.
    ; Parameters:
    ;   B - Character to print
    ; Returns:
    ;   A  - 0 if no shift on the view is required, 1 else
    ;   HL - Right address + 1 (when A = 1)
    ;   BC  - Size of the rest of the line, from cursor to end of line (when A = 1)
    PUBLIC _buffer_generic_insert_char
_buffer_generic_insert_char:
    ; - Total length has not been reached (i.e. left_addr != right_addr)
    ld hl, (_buffer_right_addr)
    ld de, (_buffer_left_addr)
    xor a
    sbc hl, de
    ret z
    ; Line is not full, neither is the buffer, add the character. Put left
    ; cursor address in HL (currently in DE)
    ex de, hl
    ld (hl), b
    ; Increment the left cursor and store it back
    inc hl
    ld (_buffer_left_addr), hl
    ; Before incrementing the rest of the fields, calculate the size of the right buffer. In other words,
    ; the number of characters on the same line but after the cursor
    call _buffer_size_right_line
    push hl
    ; Increment line's length, no need to check for overflow (already done)
    ld hl, (_buffer_cur_length)
    inc hl
    ld (_buffer_cur_length), hl
    ; Increment the col number
    ld hl, (_buffer_col_num)
    inc hl
    ld (_buffer_col_num), hl
    ; Return the right buffer address if the (right) size is not zero
    pop bc
    ld a, b
    or c
    ret z
    ld hl, (_buffer_right_addr)
    inc hl
    ret


    ; Insert the character in the split buffer, at the cursor position
    ; Tested: OK!
    ; Parameters:
    ;   B - Character to print
    ; Alters:
    ;   A, BC, DE, HL
    PUBLIC _buffer_insert_char_at_cursor
_buffer_insert_char_at_cursor:
    ; Check that the current line length has not exceed the maximum size
    ld hl, (_buffer_cur_length)
    ld de, BUFFER_MAX_LINE_LENGTH
    or a
    sbc hl, de
    ; The current line is full, do nothing
    ret z
    ; We have to save BC as B contains the character to print!
    push bc
    ; Call the generic function that pushes the char into the buffer
    ; and returns the right_address + 1 in HL and the length of the rest
    ; of the line in BC
    call _buffer_generic_insert_char
    ; If returned A is 0, no need to shift the view
    or a
    jp z, _insert_no_shift
    ; Pop the character to print in A
    pop af
    ; Modify the behavior here to simplify writing to the screen
    dec hl
    ld (hl), a
    ; More character to print
    inc bc
    jp _editor_view_insert_string_at_cursor
_insert_no_shift:
    ; Pop the character to print in A
    pop af
    jp _editor_view_insert_at_cursor


    ; Remove the char currently pointed by the cursor
    PUBLIC _buffer_remove_char_at_cursor
_buffer_remove_char_at_cursor:
    ; Make sure the cursor is not at the beginning (this also happens if the line is empty)
    ; TODO: Support going to previous line here too
    ld hl, (_buffer_col_num)
    ld a, h
    or l
    ret z
    ; Before modifying the properties of the buffer, calculate the size of the right buffer
    call _buffer_size_right_line
    push hl
    ; Decrement the column
    ld hl, (_buffer_col_num)
    dec hl
    ld (_buffer_col_num), hl
    ; Decrement the line length
    ld hl, (_buffer_cur_length)
    dec hl
    ld (_buffer_cur_length), hl
    ; Line is not empty, decrement left pointer
    ld hl, (_buffer_left_addr)
    dec hl
    ld (_buffer_left_addr), hl
    ; To print this character to the view, we have to shift all the line left.
    ; It shall not be done if the size of the right buffer is 0.
    pop bc
    ld a, b
    or c
    jp z, _editor_view_remove_at_cursor
    ; We have to shift BC characters (exclude \n) from right_addr + 1
    ld hl, (_buffer_right_addr)
    inc hl
    jp _editor_view_insert_string_before_cursor


    ; Move the cursor of the buffer left down to the given cursor
    ; No check will be performed here.
    ; Parameters:
    ;   HL - New cursor index
    ; Returns:
    ;   -
    PUBLIC model_move_cursor_left_to
model_move_cursor_left_to:
    ld de, (_buffer_col_num)
    ex de, hl
    or a
    sbc hl, de
    ret z
    ld (_buffer_col_num), de
    ld b, h
    ld c, l
    ld hl, (_buffer_left_addr)
    dec hl
    ld de, (_buffer_right_addr)
    lddr
    inc hl
    ld (_buffer_left_addr), hl
    ld (_buffer_right_addr), de
    ret


    ; Make the buffer cursor point to the previous character.
    ; This won't change the size of course.
    PUBLIC _buffer_previous_char
_buffer_previous_char:
    ; Cursor shall not be at the beginning!
    ; TODO: Support going to the previous line
    ld hl, (_buffer_col_num)
    ld a, h
    or l
    ret z
    ; Decrement the cursor col position
    dec hl
    ld (_buffer_col_num), hl
    ; We can go back safely, save the new address of the left cursor,
    ; put the character in the right cursor and decrement right cursor.
    ld hl, (_buffer_left_addr)
    dec hl
    ld (_buffer_left_addr), hl
    ld a, (hl)
    ld hl, (_buffer_right_addr)
    ld (hl), a
    dec hl
    ld (_buffer_right_addr), hl
    ; Update the view accordingly
    jp _editor_view_left_cursor


    ; Make the buffer cursor point to the next character.
    PUBLIC _buffer_next_char
_buffer_next_char:
    ; Same as the previous routine but with right cursor
    ld de, (_buffer_right_addr)
    ld hl, BUFFER_END_ADDRESS
    ; Clear carry flag
    or a
    ; 16-bit 'add' doesn't update Z flag
    sbc hl, de
    ret z
    ; Put back the right address in HL
    ex de, hl
    ; Check that the next char is NOT a new line
    ; TODO: Go to the next line
    inc hl
    ld a, (hl)
    cp '\n'
    ret z
    ; We can go forward safely, save the new address of the right cursor,
    ; put the character in the left cursor and increment the left cursor.
    ld (_buffer_right_addr), hl
    ld hl, (_buffer_left_addr)
    ld (hl), a
    inc hl
    ld (_buffer_left_addr), hl
    ; Update the column number
    ld hl, (_buffer_col_num)
    inc hl
    ld (_buffer_col_num), hl
    ; Update the view accordingly
    jp _editor_view_right_cursor


    ; Insert a new line in the buffer at the cursor position.
    ; Tested: OK!
    ; Parameters:
    ;   B - '\n' char value
    PUBLIC _buffer_insert_new_line
_buffer_insert_new_line:
    ; Call the generic function that pushes \n into the buffer
    ; and returns the right_address + 1 in HL and the length of the rest
    ; of the line in BC
    call _buffer_generic_insert_char
    ; Store BC, rest of the line in _buffer_cur_length
    ld (_buffer_cur_length), bc
    push bc
    ; If returned A is 0, no need to shift the view
    or a
    call nz, _editor_view_clean_string_at_cursor
    ; Add a line to the count
    ld hl, (_buffer_line_count)
    inc hl
    ld (_buffer_line_count), hl
    ; Reset column number
    ld hl, 0
    ld (_buffer_col_num), hl
    ; Increment the line index
    ld hl, (_buffer_line_num)
    inc hl
    ld (_buffer_line_num), hl
    ; Retrieve the parameters from the stack and jump to the view routine
    pop bc
    ld hl, (_buffer_right_addr)
    inc hl
    jp _editor_view_goto_newline


    ; Get the address of the next line
    ; Parameters:
    ;   None
    ; Returns:
    ;   HL - Address of the next line
    ; Alters:
    ;   A, BC, HL
    PUBLIC model_next_line_address
model_next_line_address:
    ; Get the size of the line in the right buffer
    call _buffer_size_right_line
    ex de, hl
    ld hl, (_buffer_right_addr)
    ; Increment HL to point to the next valid character
    inc hl
    ; Set the carry to add 1 in the calculation below, to skip the \n
    scf
    adc hl, de
    ret


    ; Get the number of lines that follow the cursor, including the current one
    ; Tested: OK!
    ; Parameters:
    ;   -
    ; Returns:
    ;   HL - Number of lines remaining
    ; Alters:
    ;   A, HL, DE
    PUBLIC model_get_remaining_lines
model_get_remaining_lines:
    ld hl, (_buffer_line_count)
    ld de, (_buffer_line_num)
    xor a
    sbc hl, de
    ret


    ; Retrieve the the A-th previous line of the current buffer pointer
    ; Parameters:
    ;    A - Index of the previous line to get (0 = current line, 1 = previous line, etc...)
    ; Returns:
    ;   HL - Address of the A-th previous line
    ; Alters:
    ;   A, HL, BC, E
    PUBLIC _buffer_previous_ath_line_address
_buffer_previous_ath_line_address:
    ld hl, (_buffer_left_addr)
    dec hl
    ld e, a
    inc e
    ld a, '\n'
_buffer_previous_ath_line_address_loop:
    ld bc, BUFFER_MAX_LINE_LENGTH + 1
    cpdr
    ret nz   ; Should not happen
    dec e
    jp nz, _buffer_previous_ath_line_address_loop
    ; After CPDR, HL points to the char before the \n that was found,
    ; we want it to point to the beginning of the line, so increment it twice.
    inc hl
    inc hl
    ret


    ; Calculate the minimum between HL and BC and put the result in BC
    ; Parameters:
    ;   HL - 16-bit value
    ;   BC - 16-bit value
    ; Returns:
    ;   BC - Minimum between HL and BC
    ; Alters:
    ;   HL, BC
_buffer_min_hl_bc:
    or a    ; Clear carry flag
    sbc hl, bc
    ret nc
    ; HL was the minimum, restore it and store it in BC
    add hl, bc
    ld b, h
    ld c, l
    ret


    ; Get a pointer to characters located on the left of the
    ; cursor.
    ; The parameter BC must not exceed the size of the buffer,
    ; no check is performed here!
    ; Parameters:
    ;   BC - Number of characters to get
    ; Returns:
    ;   HL - Pointer to the string
    ; Alters:
    ;   A, HL, BC
    PUBLIC model_get_bc_left_chars
model_get_bc_left_chars:
    ld hl, (_buffer_left_addr)
    or a    ; clear carry flag
    sbc hl, bc
    ret

    ; Get a pointer to characters located on the right of the
    ; cursor.
    ; Parameters:
    ;   BC - Request size
    ; Returns:
    ;   HL - Pointer to the string
    ;   BC - Minimum between requested size and size of the rest of the line
    ; Alters:
    ;   HL
    PUBLIC model_get_bc_right_chars
model_get_bc_right_chars:
    push bc
    call _buffer_size_right_line
    pop bc
    call _buffer_min_hl_bc
    ld hl, (_buffer_right_addr)
    inc hl
    ret


    ; Fills the given buffer with current line content.
    ; Parameters:
    ;   DE - Buffer to fill with the current line content
    ;   BC - Maximum number of bytes to fill in DE
    ; Returns:
    ;   DE - End of the buffer
    ;   BC - Number of characters written
    ; Alters:
    ;   A, BC, DE, HL
    PUBLIC model_get_current_line
model_get_current_line:
    push bc
    ; Copy 'col' characters from left iterator
    ld hl, (_buffer_col_num)
    ld a, h
    or l
    jr z, model_get_current_line_right
    ; Make HL point to the beginning of the line
    push de
    ld de, (_buffer_left_addr)
    ex de, hl
    sbc hl, de  ; Carry is 0 here
    ex de, hl
    ; HL - contains 'col' number
    ; BC - max number to write to caller buffer
    ; DE - start of current line buffer
    ; [SP] - User buffer
    ; Keep BC on the stack, and get the minimum between BC and HL, store it in BC
    push bc
    call _buffer_min_hl_bc
    ; Before actually start copying, subtract BC from the user buffer size
    pop hl
    xor a
    sbc hl, bc
    ; HL contains the user buffer remaining size after copying. Exchange it with user buffer.
    ex (sp), hl
    ; Copy BC bytes from current line (DE) to the user buffer (HL)
    ex de, hl
    ldir
    pop bc
    ; BC - Remaining size in user buffer
    ; DE - User buffer next free byte
    ; If there is no space anymore, return
    ld a, b
    or c
    jr z, model_get_current_line_return
    ; Calculate for the right buffer now
    ld hl, (_buffer_col_num)
model_get_current_line_right:
    ; HL contains column index, calculate the length of the line in the right buffer
    push de
    ld de, (_buffer_cur_length)
    ex de, hl
    xor a
    sbc hl, de
    pop de
    ; If the result is 0, we have nothing more to write to the buffer
    jr z, model_get_current_line_calculate_return
    ; Calculate the minimum between HL (line size in right buffer) and BC (user buffer remaining size)
    call _buffer_min_hl_bc
    ; We have to write BC characters from right buffer
    ld hl, (_buffer_right_addr)
    inc hl
    ldir
    ; The total amount of bytes written is MIN(line_length, user_buffer_size)
    ld hl, (_buffer_cur_length)
    pop bc
    jp _buffer_min_hl_bc
model_get_current_line_return:
    ; Pop original buffer size, to say that we filled the buffer
    pop bc
    ret
model_get_current_line_calculate_return:
    ; Calculate user buffer original size - remaining size
    pop hl
    ; Carry is 0 if we reached this point
    sbc hl, bc
    ld b, h
    ld c, l
    ret


    ; Calculate the length of the previous line.
    ; Parameters:
    ;   HL - Address of the \n finalizing the previous line
    ; Returns:
    ;   HL - Length of the previous line
_buffer_previous_line_length:
    ; Go past the previous line's \n
    dec hl
    ; The previous line is preceded by a \n, look for it
    ld bc, BUFFER_MAX_LINE_LENGTH + 1
    ld a, '\n'
    cpdr
    ; When found, length of line is BUFFER_MAX_LINE_LENGTH - BC
    ld hl, BUFFER_MAX_LINE_LENGTH
    sbc hl, bc
    ret


    ; Make the buffer cursor point to the previous line.
    ; If the previous line has less character than current line, the cursor
    ; will be placed at the end of the previous line.
    ; Parameters:
    ;   None
    ; Returns:
    ;   None
    PUBLIC _buffer_previous_line
_buffer_previous_line:
    ; Make sure we are not at the first line
    ld hl, (_buffer_line_num)
    ld a, h
    or l
    ; Return if HL is 0 (meaning we are at the first line)
    ret z
    ; Update _buffer_line_num now
    dec hl
    ld (_buffer_line_num), hl
    ; Load the pointers
    ld hl, (_buffer_left_addr)
    dec hl
    ld de, (_buffer_right_addr)
    ld bc, (_buffer_col_num)
    ; Copy BC bytes from left buffer into right buffer
    ld a, b
    or c
    jr z, _buffer_previous_line_no_copy
    lddr
_buffer_previous_line_no_copy:
    push hl
    call _buffer_previous_line_length
    ; Store the current line length
    ld (_buffer_cur_length), hl
    ; If the new line has the same amount or more characters than the current cursor index, do not move it,
    ; else, move it to the end of the line.
    ld bc, (_buffer_col_num)
    ; If the cursor is at the end, trigger a carry flag during 'sbc'
    scf
    sbc hl, bc
    jr nc, _buffer_previous_line_cursor_valid
    ; BC is bigger than the size, set the cursor to the end of the line
    adc hl, bc
    ld (_buffer_col_num), hl
    ; Pop the left buffer index from the stack
    pop hl
    ; Copy the ending \n, we can alter BC, it won't be used later
    ldd
    jp _buffer_previous_line_cursor_no_copy
_buffer_previous_line_cursor_valid:
    ; HL contains size - cursor, copy that many bytes into right buffer (DE)
    ld b, h
    ld c, l
    ; Increment BC because we subtracted a carry too
    inc bc
    ; Increment again to integrate the previous line's \n in the copy
    inc bc
    ; Pop the left buffer index from the stack
    pop hl
    lddr
_buffer_previous_line_cursor_no_copy:
    ; Make HL point to the next available spot
    inc hl
    ld (_buffer_left_addr), hl
    ld (_buffer_right_addr), de
    ld bc, (_buffer_col_num)
    jp _editor_view_cursor_prev_line


    ; Calculate the length of the next line.
    ; Parameters:
    ;   HL - Address of the first character of next line
    ; Returns:
    ;   HL - Length of the next line
_buffer_next_line_length:
    ; The previous line is preceded by a \n, look for it
    ld bc, BUFFER_MAX_LINE_LENGTH + 1
    ld a, '\n'
    cpir
    ; When found, length of line is BUFFER_MAX_LINE_LENGTH - BC
    ld hl, BUFFER_MAX_LINE_LENGTH
    sbc hl, bc
    ret


    ; Make the buffer cursor point to the next line.
    ; If the next line has less character than current line, the cursor
    ; will be placed at the end of the next line.
    ; Parameters:
    ;   None
    ; Returns:
    ;   None
    PUBLIC _buffer_next_line
_buffer_next_line:
    ; Make sure we are not at the last line
    ld bc, (_buffer_line_num)
    ld hl, (_buffer_line_count)
    scf
    sbc hl, bc
    ; Return if HL is the last line
    ret z
    ; Update _buffer_line_num now
    inc bc
    ld (_buffer_line_num), bc
    ; Calculate the number of bytes to send to the end of the buffer, in other
    ; words, make the cursor point to the end of the current line:
    call _buffer_size_right_line
    ; It doesn't count the \n, so add it
    inc hl
    ld b, h
    ld c, l
    ; Perform the copy
    ld de, (_buffer_left_addr)
    ld hl, (_buffer_right_addr)
    inc hl
    ldir
    ; Look for the length of the next line, HL is already pointing to its first character
    push hl
    call _buffer_next_line_length
    ; Store the current line length
    ld (_buffer_cur_length), hl
    ld bc, (_buffer_col_num)
    ; Now, we have to transfer MIN(length, cursor) characters <=> MIN(HL, BC)
    call _buffer_min_hl_bc
    ld (_buffer_col_num), bc
    ; Pop the right buffer index from the stack
    pop hl
    ; Check if BC is 0 (cursor at the beginning already)
    ld a, b
    or c
    jp z, _buffer_next_line_cursor_no_copy
    ldir
_buffer_next_line_cursor_no_copy:
    ; HL points to the right buffer (source)
    dec hl
    ld (_buffer_right_addr), hl
    ld (_buffer_left_addr), de
    ld bc, (_buffer_col_num)
    jp _editor_view_cursor_next_line


    ; Save the current buffer to a file
    PUBLIC model_save_file
model_save_file:
    ld hl, str_saving
    call editor_show_notification
    ld bc, (_model_full_path)
    ld h, O_WRONLY | O_CREAT | O_TRUNC
    OPEN()
    ; If an error occurred, return
    or a
    jp m, model_print_error_neg
    ; There are two parts to write to the file:
    ;   - From the beginning to the cursor
    ;   - From the cursor to the end
    ld hl, (_buffer_left_addr)
    ld de, BUFFER_START_ADDRESS
    or a    ; clear carry flag
    sbc hl, de
    ld b, h
    ld c, l
    ex de, hl
    push af ; Save the opened descriptor
    call model_save_buffer
    pop de
    or a
    jr nz, model_print_error
    ; Get back the opened descriptor
    ld a, d
    ; Save for the right buffer now
    ld de, BUFFER_END_ADDRESS
    ld hl, (_buffer_right_addr)
    ex de, hl
    sbc hl, de  ; carry flag is already 0
    ld b, h
    ld c, l
    ex de, hl
    inc hl
    push af
    call model_save_buffer
    pop hl
    or a
    jr nz, model_print_error
    ; Close the file descriptor in H
    CLOSE()
    ; Success!
    ld hl, str_success
    jp editor_show_notification

model_print_error_neg:
    neg
model_print_error:
    ld hl, str_error_code
    call model_show_location_to_decimal
    ld (hl), 0
    ld hl, str_error_msg
    jp editor_show_notification

str_saving: DEFM "Saving...", 0
str_success: DEFM "Success", 0
str_error_msg:  DEFM "Error saving file: "
str_error_code: DEFS 3
                DEFB 0


    ; Save the given buffer of given size to the opened descriptor (file)
    ; Parameters:
    ;   A - Opened descriptor
    ;   HL - Buffer containing data to write
    ;   BC - Size of the buffer
    ; Returns:
    ;   A - ERR_SUCCESS on success, error code else
    ; Alters:
    ;   A, BC, DE, HL
model_save_buffer:
    ld (_model_file_dev), a
model_save_buffer_descriptor_saved:
    ; If BC is 0, return success directly
    ld a, b
    or c
    ret z
    ; Check if the buffer goes beyond one virtual page (the kernel would refuse it else)
    push hl
    ld a, h
    ; Get the virtual page of HL and store it in D
    and 0xc0
    ld d, a
    ; Get the address of the last char of the buffer
    dec hl
    add hl, bc
    ; Get the virtual page of this new address
    ld a, h
    and 0xc0
    ld h, a
    ; If it is the same as the former one, the buffer doesn't cross-boundary, we have to call
    ; WRITE syscall only once
    cp d
    jr z, model_save_buffer_not_cross_boundary
    ; The buffer crosses boundaries, at most 3, the first address to write is the original one, but
    ; we need to modify the size.
    pop hl  ; Original buffer address
    ; Calculate the boundary address out of the virtual page count
    ld a, d
    add 0x40    ; the virtual page is not 3 for sure, so increment it by 1
    ld d, a
    ld e, 0
    push de     ; save next virtual size to write
    ; DE contains the end of page address, calculate the new size
    push bc ; Save original size
    ex de, hl
    or a
    sbc hl, de
    ; Size is in HL
    ld b, h
    ld c, l
    ; Buffer is in DE, write now
    call model_save_buffer_not_cross_boundary_buffer_in_de
    ; Calculate remaining size to write
    pop hl
    or a
    sbc hl, bc
    ; Put the remaining size in BC
    ld b, h
    ld c, l
    ; Pop the next buffer to write
    pop hl
    ; If there was no error, continue writing
    or a
    jp z, model_save_buffer_descriptor_saved
    ; Else, return directly
    ret

    ; Parameters:
    ;   (_model_file_dev) - Opened descriptor
    ;   [SP] - Address of the buffer to print
    ;   BC - Size of the buffer to print
    ; Returns:
    ;   BC - Number of bytes written
model_save_buffer_not_cross_boundary:
    ; Buffer to write to file in DE
    pop de
model_save_buffer_not_cross_boundary_buffer_in_de:
    ; Opened descriptor in H
    ld a, (_model_file_dev)
    ld h, a
    WRITE()
    ret

    ; Show the current current of the cursor in the notification bar
    PUBLIC model_show_location
model_show_location:
    ld de, loc_line
    ; Since the line_num is an index, we have to increment it
    ld hl, (_buffer_line_num)
    inc hl
    call model_show_location_to_decimal
    ; Same goes for the column
    ld (hl), ','
    inc hl
    ld (hl), ' '
    inc hl
    ld (hl), 'C'
    inc hl
    ld (hl), 'o'
    inc hl
    ld (hl), 'l'
    inc hl
    ld (hl), ' '
    inc hl
    ex de, hl
    ld hl, (_buffer_col_num)
    inc hl
    call model_show_location_to_decimal
    ; Clean the end of the buffer
    ld (hl), 0
    ; Finally, we can print the resulting buffer
    ld hl, loc_start
    jp editor_show_notification


    ; Convert a 16-bit value to decimal (ASCII) and store it in the
    ; buffer pointed by DE
    ; Parameters:
    ;   DE - Buffer to store the result in
    ;   HL - 16-bit value to convert
    ; Returns:
    ;   HL - Address of the last digit + 1
model_show_location_to_decimal:
    ld c, 10    ; Constant, not modified by the code
    push de
    pop ix      ; Move DE in IX
    ld d, 0     ; Number of digits
model_show_location_to_decimal_loop:
    call _model_divide_hl_c
    ; Remainder in A, convert to ASCII
    add '0'
    push af
    inc d
    ; Check if HL is 0
    ld a, h
    or l
    jp nz, model_show_location_to_decimal_loop
    ; We have D digits, iterate over all the digits
    ld b, d
    push ix
    pop hl
model_show_location_to_decimal_pop_loop:
    pop af
    ld (hl), a
    inc hl
    djnz model_show_location_to_decimal_pop_loop
    ret


loc_start: DEFM "Ln "
loc_line:  DEFS 20


    ; Divide a 16-bit value by an 8-bit value
    ; Parameters:
    ;   HL - Number of divide
    ;   C  - Divider
    ; Returns:
    ;   HL - Result
    ;   A  - Remainder
_model_divide_hl_c:
   xor a
   ld b, 16
_model_divide_hl_c_loop:
   add hl, hl
   rla
   jp c, _model_divide_hl_carry
   cp c
   jp c, _model_divide_hl_next
_model_divide_hl_carry:
   sub c
   inc l
_model_divide_hl_next:
   djnz _model_divide_hl_c_loop
   ret


    SECTION DATA
_model_full_path: DEFS 2
_model_file_dev: DEFS 1
    ; Split buffer related
    ; Current length of the line, in bytes
_buffer_cur_length: DEFS 2
    ; Contains the line number the cursor is currently at
_buffer_line_num: DEFS 2
    ; Same for the column
_buffer_col_num: DEFS 2
    ; Contains the number of lines
_buffer_line_count: DEFS 2
    ; Contains the address of the LEFT cursor in the file buffer
_buffer_left_addr: DEFS 2
    ; Same but for the RIGHT cursor
_buffer_right_addr: DEFS 2
