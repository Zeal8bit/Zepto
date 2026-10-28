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
    EXTERN _editor_view_join_previous_line
    EXTERN _editor_view_join_next_line
    EXTERN _editor_view_redraw_current_line_and_restore
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
    ; DEFC BUFFER_MAX_SIZE = 32*1024
    ; DEFC BUFFER_END_ADDRESS = _start_buffer + BUFFER_MAX_SIZE - 1
    DEFC BUFFER_MAX_LINE_LENGTH = 0xFFFF
    DEFC MODEL_SAVE_CHUNK = 128

    DEFC EDITOR_MAX_LINE = (40 - 1) - 4

    ; Set the end memory address. Used to determine how many bytes our work
    ; buffer will have.
    ; It also determines how big can be the file we are going to work on can be.
    ; Parameters:
    ;   HL - End of memory address (last usable byte)
    PUBLIC editor_set_end_memory
editor_set_end_memory:
    ; The last byte will be a '\n' to simplify the calculations, so reserve one byte
    dec hl
    ld (_buffer_memory_end), hl
    ret


    ; Initialize the buffers and read the file if any was given.
    ; Parameters:
    ;   DE - File name to open
    PUBLIC editor_model_init
editor_model_init:
    ld hl, _start_buffer
    ld (_buffer_left_addr), hl
    ld hl, (_buffer_memory_end)
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
    ; Make sure the file size doesn't go above the limit.
    ; The stat structure starts with a flags byte, followed by a 32-bit
    ; little-endian size. Only files fitting in 16 bits can be opened, as the
    ; buffer itself is smaller than 64KB.
    ex de, hl
    inc hl          ; Skip the flags field
    ld e, (hl)
    inc hl
    ld d, (hl)
    inc hl
    ld a, (hl)
    inc hl
    or (hl)
    jp nz, _editor_model_init_error
    ; DE contains the size of the file, calculate the maximum file size in HL:
    ; HL = BUFFER_END_ADDRESS - _start_buffer + 1
    ; BC is free to use here
_free_size:
    ld bc, -_start_buffer + 1
    ld hl, (_buffer_memory_end)
    add hl, bc
    ; HL now contains the maximum number of bytes
    or a
    sbc hl, de
    jp c, _editor_model_init_error
    ; TODO: check if the entry is a directory (once d_flags is part of stat structure)
    ; Now that the file is known to fit in the buffer, read it from the
    ; beginning of the buffer. The kernel requires a read buffer to stay
    ; within a single virtual page, so read at most up to the next page
    ; boundary on each iteration.
    ld (_model_file_size), de
    ld hl, 0
    ld (_model_bytes_read), hl
_editor_model_read_loop:
    ; HL = number of bytes left to read
    ld hl, (_model_file_size)
    ld bc, (_model_bytes_read)
    or a
    sbc hl, bc
    jr z, _editor_model_read_done
    ld (_model_remaining), hl
    ; DE = destination address = BUFFER_START + bytes read
    ld hl, BUFFER_START_ADDRESS
    add hl, bc
    ex de, hl
    ; BC = number of bytes until the end of the current virtual page
    ld a, d
    and 0xc0
    add 0x40
    ld b, a
    ld c, 0
    push de
    ld h, b
    ld l, c
    or a
    sbc hl, de
    ld b, h
    ld c, l
    pop de
    ; Read at most the remaining file size or the page boundary, whichever
    ; comes first
    ld hl, (_model_remaining)
    call _buffer_min_hl_bc
    ld a, (_model_file_dev)
    ld h, a
    READ()
    or a
    jp nz, _editor_model_init_error
    ; Stop if the host returned no data
    ld a, b
    or c
    jr z, _editor_model_read_done
    ; Account for the bytes read and continue
    ld hl, (_model_bytes_read)
    add hl, bc
    ld (_model_bytes_read), hl
    jr _editor_model_read_loop

_editor_model_read_done:
    ; We can now close the file
    ld a, (_model_file_dev)
    ld h, a
    CLOSE()
    ; Copy the file content to the end of the buffer
    ld bc, (_model_bytes_read)
    ld de, (_buffer_memory_end)
    ; An empty file has nothing to copy. LDDR with BC = 0 would copy 64KB.
    ld a, b
    or c
    jr z, _editor_model_file_copied
    ; Calculate the address of the last byte of the loaded file
    ld hl, BUFFER_START_ADDRESS
    add hl, bc
    ; HL points to the next free character, decrement it to point to the last char of the file
    dec hl
    lddr
_editor_model_file_copied:
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
    ld hl, (_buffer_memory_end)
    dec hl  ; - 1 because we want C to be set if equal
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
    or a    ; Clear carry before subtracting
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


    ; Join the current line with the previous one. The cursor must be on the
    ; first column and the current line must not be the first one.
    ; This removes the '\n' separating both lines and puts the cursor at the
    ; end of the previous line, as if the user had pressed BACKSPACE there.
    ; Parameters:
    ;   None
    ; Returns:
    ;   None
    PUBLIC _buffer_join_previous_line
_buffer_join_previous_line:
    ; Update the line index and the total number of lines
    ld hl, (_buffer_line_num)
    dec hl
    ld (_buffer_line_num), hl
    ld hl, (_buffer_line_count)
    dec hl
    ld (_buffer_line_count), hl
    ; The separator to delete is the last character on the left side of the gap
    ld hl, (_buffer_left_addr)
    dec hl
    push hl
    ; Calculate the length of the previous line, it becomes the new column
    call _buffer_previous_line_length
    ld (_buffer_col_num), hl
    ; The merged line length is the sum of both line lengths
    ld de, (_buffer_cur_length)
    add hl, de
    ld (_buffer_cur_length), hl
    ; Delete the '\n' by moving the gap over it
    pop hl
    ld (_buffer_left_addr), hl
    ; Redraw the merged line and the lines that follow it
    jp _editor_view_join_previous_line


    ; Join the next line into the current one. The cursor must be at the end
    ; of the current line and a next line must exist.
    PUBLIC _buffer_join_next_line
_buffer_join_next_line:
    ; Remove one line from the count
    ld hl, (_buffer_line_count)
    dec hl
    ld (_buffer_line_count), hl
    ; The next line starts right after the '\n' located at right_addr + 1
    ld hl, (_buffer_right_addr)
    inc hl
    inc hl
    call _buffer_next_line_length
    ; The merged line is the sum of both line lengths
    ld de, (_buffer_cur_length)
    add hl, de
    ld (_buffer_cur_length), hl
    ; Delete the '\n' by moving the gap over it
    ld hl, (_buffer_right_addr)
    inc hl
    ld (_buffer_right_addr), hl
    ; Redraw the merged line and the lines that follow it
    jp _editor_view_join_next_line


    ; Move the cursor to the beginning of the current line.
    PUBLIC model_cursor_home
model_cursor_home:
    ld hl, (_buffer_col_num)
    ld a, h
    or l
    jr z, _model_cursor_home_redraw
    ld hl, 0
    call model_move_cursor_left_to
_model_cursor_home_redraw:
    jp _editor_view_redraw_current_line_and_restore


    ; Move the cursor to the end of the current line.
    PUBLIC model_cursor_end
model_cursor_end:
    ld hl, (_buffer_cur_length)
    call model_move_cursor_right_to
    jp _editor_view_redraw_current_line_and_restore


    ; Remove the character under the cursor. At the end of the line, join the
    ; next line into the current one instead. Do nothing on the last line.
    PUBLIC model_delete
model_delete:
    ld hl, (_buffer_col_num)
    ld de, (_buffer_cur_length)
    or a
    sbc hl, de
    ; If col < cur_length, there is a character to delete on this line
    jr c, _model_delete_char
    ; At the end of the line, check that a next line exists
    ld hl, (_buffer_line_num)
    inc hl
    ld de, (_buffer_line_count)
    or a
    sbc hl, de
    ret nc
    jp _buffer_join_next_line

_model_delete_char:
    ; Move the cursor over the character, then delete it as a backspace would
    call _buffer_next_char
    jp _buffer_remove_char_at_cursor


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


    ; Move the cursor of the buffer right to the given column.
    ; The given column must be between the current column and the current line
    ; length (inclusive). No check is performed here.
    ; Parameters:
    ;   HL - New cursor index
    ; Returns:
    ;   None
model_move_cursor_right_to:
    push hl                 ; Save the target column
    ld de, (_buffer_col_num)
    or a
    sbc hl, de              ; HL = number of characters to move
    jr z, _model_move_cursor_right_done
    jr c, _model_move_cursor_right_abort
    ld b, h
    ld c, l
    ; LDIR copies from HL to DE: source is the right side, destination is the
    ; left side.
    ld de, (_buffer_left_addr)      ; Destination
    ld hl, (_buffer_right_addr)
    inc hl                          ; Source
    ldir
    ld (_buffer_left_addr), de
    dec hl
    ld (_buffer_right_addr), hl
_model_move_cursor_right_done:
    pop hl
    ld (_buffer_col_num), hl
    ret
_model_move_cursor_right_abort:
    pop hl
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
    ld hl, (_buffer_memory_end)
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
    or a    ; Clear carry, CPDR doesn't guarantee it
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
    or a    ; Clear carry, CPIR doesn't guarantee it
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
    ; Remove the destination first. On HostFS, truncating an existing file
    ; larger than 256 bytes leaves the old size in the opened file structure,
    ; which makes the following writes fail. Removing it makes the OPEN below
    ; create a fresh file. The result is ignored as the file may not exist.
    ld de, (_model_full_path)
    RM()
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
    ld de, (_buffer_memory_end)
    ld hl, (_buffer_right_addr)
    ex de, hl
    or a    ; Clear carry before subtracting
    sbc hl, de
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
    ; Convert the error code in A to decimal in str_error_code
    ld l, a
    ld h, 0
    ld de, str_error_code
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
    ld (_save_ptr), hl
    ld (_save_left), bc
_model_save_loop:
    ; Stop when there is nothing left to write
    ld hl, (_save_left)
    ld a, h
    or l
    ret z
    ; chunk = min(remaining, MODEL_SAVE_CHUNK)
    ld de, MODEL_SAVE_CHUNK
    push hl
    or a
    sbc hl, de
    pop hl
    jr c, _model_save_chunk_size_ready
    ld hl, de
_model_save_chunk_size_ready:
    ; Cap the chunk to the space left in the current virtual page
    push hl                     ; Save chunk
    ld hl, (_save_ptr)
    ld a, h
    and 0xc0
    add 0x40
    ld d, a
    ld e, 0                     ; DE = end of the current virtual page
    ex de, hl                   ; DE = buffer, HL = page end
    or a
    sbc hl, de                  ; HL = page space
    ex de, hl                   ; DE = page space, HL = buffer
    pop bc                      ; BC = chunk
    push hl                     ; Save buffer
    ld h, d
    ld l, e                     ; HL = page space
    or a
    sbc hl, bc                  ; page space - chunk
    jr nc, _model_save_chunk_ok ; page space >= chunk
    ld b, d
    ld c, e                     ; chunk = page space
_model_save_chunk_ok:
    ld (_save_chunk), bc
    pop de                      ; DE = buffer
    ld a, (_model_file_dev)
    ld h, a
    WRITE()
    or a
    ret nz
    ; Account for the bytes actually written and continue
    ld hl, (_save_ptr)
    add hl, bc
    ld (_save_ptr), hl
    ld hl, (_save_left)
    or a
    sbc hl, bc
    ld (_save_left), hl
    jr _model_save_loop


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
    PUBLIC _model_divide_hl_c
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
_save_ptr: DEFS 2
_save_left: DEFS 2
_save_chunk: DEFS 2
    ; File loading state: total size, bytes already read, bytes left
_model_file_size: DEFS 2
_model_bytes_read: DEFS 2
_model_remaining: DEFS 2
    ; Split buffer related
    ; Current length of the line, in bytes
    PUBLIC _buffer_cur_length
_buffer_cur_length: DEFS 2
    ; Contains the line number the cursor is currently at
    PUBLIC _buffer_line_num
_buffer_line_num: DEFS 2
    ; Same for the column
    PUBLIC _buffer_col_num
_buffer_col_num: DEFS 2
    ; Contains the number of lines
_buffer_line_count: DEFS 2
    ; Contains the address of the LEFT cursor in the file buffer
_buffer_left_addr: DEFS 2
    ; Same but for the RIGHT cursor
_buffer_right_addr: DEFS 2
    ; Last valid memory address, in other words, maximum value of _buffer_right_addr
    PUBLIC _buffer_memory_end
_buffer_memory_end: DEFS 2