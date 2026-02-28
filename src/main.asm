; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    INCLUDE "zos_keyboard.asm"
    INCLUDE "zos_video.asm"

    ; Linker script part
    SECTION TEXT
    ORG 0x4000
    SECTION DATA
    SECTION BUFFER
    ; Program part


    EXTERN editor_set_end_memory
    EXTERN editor_view_init
    EXTERN editor_model_init
    EXTERN model_show_location
    EXTERN model_save_file
    EXTERN _buffer_insert_char_at_cursor
    EXTERN _buffer_insert_new_line
    EXTERN _buffer_remove_char_at_cursor
    EXTERN _buffer_previous_char
    EXTERN _buffer_next_char
    EXTERN _buffer_previous_line
    EXTERN _buffer_next_line
    EXTERN editor_show_notification

    DEFC FLAGS_CAPS_MASK  = 1 << 0
    DEFC FLAGS_SHIFT_MASK = 1 << 1
    DEFC FLAGS_CTRL_MASK  = 1 << 2

    ; Entry point of the program
    ; Parameters:
    ;   DE - String parameter (NULL-terminated), must be ignored if BC is 0
    ;   BC - Length of the string parameter
    SECTION TEXT
_start:
    ; Before processing any parameter, let's estimate the size of free RAM we have.
    ; As this program can be executed on a target than has no MMU, the stack pointer
    ; may not be 0xFFFF. So, we need to calculate the last byte address of memory.
    ld hl, 0
    add hl, sp
    ; Allocate 256 bytes for the stack, should be enough, if any problem is encountered, it
    ; should be increased.
    dec h
    dec hl
    call editor_set_end_memory
    ; Process the argument if any
    call process_argument
    push de
    ; Start by drawing the menu bar
    call editor_view_init
    pop de
    push de
    call editor_model_init
    call _editor_controller_init
    ; If a file was provided, no need to write the test data
    pop de
    ld a, d
    or e
    jp _editor_main_loop

    ; Draw the different element of the editor, they must be updated after each action
    ; of the user
    ld ix, _controller_print_printable
    ld iy, _editor_event_process_special_ascii
_editor_main_loop:
    ; Load the routine to jump to in IX if the character received is a printable one
    ; and the one to jump to if it is a special ASCII character, in IY
    ld ix, _controller_print_printable
    ld iy, _editor_event_process_special_ascii
    call _editor_wait_for_event
    jp _editor_main_loop


    ; Parameters:
    ;   DE - Address of the NULL-terminated string
    ;   BC - Length of the string
    ; Returns:
    ;   DE - Processed argument, NULL if not valid
process_argument:
    ld a, b
    or c
    jr z, process_argument_error
    ; Left trim the parameter
    ex de, hl
    call ltrim
    jr z, process_argument_error
    ; If the argument is made out of several argument, split and keep the first
    push hl
    call strsep
    pop de
    ret
process_argument_error:
    ; Set DE to 0
    ld d, a
    ld e, a
    ret

    ; Parameters:
    ;   HL - Address of the string to separate
    ; Alters:
    ;   A, HL
strsep:
    inc hl
    ld a, (hl)
    or a
    ret z
    sub ' '
    jp nz, strsep
    ; Character is space, replace it with 0 (A)
    ld (hl), a
    ret

    ; Parameters:
    ;   HL - Address of the string to trim
    ; Returns:
    ;   HL - Address of the first non-space character
    ;   Z - The resulted string is empty
    ;   NZ - Resulted string is not empty
ltrim:
    ld a, (hl)
    or a
    ret z
    cp ' '
    ret nz
    inc hl
    jp ltrim


    ; Routine returning the length of a NULL-terminated string
    ; Parameters:
    ;   HL - NULL-terminated string to get the length from
    ; Returns:
    ;   BC - Length of the string
    ; Alters:
    ;   A, BC
    PUBLIC strlen
strlen:
    push hl
    xor a
    ld b, a
    ld c, a
_strlen_loop:
    cp (hl)
    jr z, _strlen_end
    inc hl
    inc bc
    jr _strlen_loop
_strlen_end:
    pop hl
    ret


    ; Function copying src string into dest, including the terminating null byte
    ; At most BC characters are copied
    PUBLIC strncpy
strncpy:
_strncpy_loop:
    ld a, (hl)
    ldi
    or a
    ret z
    ld a, b
    or c
    ret z
    jp _strncpy_loop


    ; Return the basename of a path
    ; Parameters:
    ;   HL - Address of the file path, which can be relative or absolute
    ; Returns:
    ;   HL - Address of the file basename
    ; Alters:
    ;   A, HL, BC
    PUBLIC basename
basename:
    ; BC will contain the address of the last slash + 1
    ld b, h
    ld c, l
_basename_loop:
    ld a, (hl)
    inc hl
    or a
    jr z, _basename_end
    cp '/'
    jr nz, _basename_loop
    ; Found a slash, copy the current incremented HL to BC
    ld b, h
    ld c, l
    jr _basename_loop
_basename_end:
    ; Copy the basename address in HL and return
    ld h, b
    ld l, c
    ret


    ;=========================================================;
    ;==================  CONTROLLER ==========================;
    ;=========================================================;

_editor_controller_init:
    ; Set STDIN to raw input
    ld h, DEV_STDIN
    ld c, KB_CMD_SET_MODE
    ld e, KB_MODE_RAW
    IOCTL()
    or a
    ret z
    ; Error, the target doesn't support RAW mode
    S_WRITE3(DEV_STDOUT, _editor_raw_err_str, _editor_raw_err_str_end - _editor_raw_err_str)
    EXIT()
_editor_raw_err_str:
    DEFM "Could not switch input to RAW mode\n"
_editor_raw_err_str_end:


is_print:
    ; Printable characters are above 0x20 (space) and below 0x7F
    cp ' '
    ret c
    cp 0x7F
    ccf
    ret


_editor_wait_for_event:
    ; Get the key pressed from the keyboard
    S_READ3(DEV_STDIN, _editor_keys, _editor_keys_end - _editor_keys)
_event_received:
    ld a, b
    or c
    jr z, _editor_wait_for_event
_editor_event_next:
    ; We received at least one character, process it
    ld a, (de)
    inc de
    cp KB_RELEASED
    jr nz, _editor_event_not_ignore
    ; Make the assumption that the released key follows directly
    dec c
    ld a, (de)
    call controller_check_if_special
    ret z
    inc de
    dec c
    ret z
    jp _editor_event_next
_editor_event_not_ignore:
    push bc
    push de
    call _editor_event_process_char
    pop de
    pop bc
    dec c
    jp nz, _editor_event_next
    ret


    ; Parameters:
    ;   A - Character to test
    ; Returns:
    ;   A - 0 if the character is not special
    ;       > 0 if it is
controller_check_if_special:
    cp KB_LEFT_SHIFT
    jr z, _controller_toggle_shift
    cp KB_RIGHT_SHIFT
    jr z, _controller_toggle_shift
    cp KB_LEFT_CTRL
    jr z, _controller_toggle_ctrl
    cp KB_RIGHT_CTRL
    jr z, _controller_toggle_ctrl
    ; Return 0, it was not a special character
    xor a
    ret

_controller_toggle_ctrl:
    ld a, FLAGS_CTRL_MASK
    jr _controller_toggle
_controller_toggle_shift:
    ld a, FLAGS_SHIFT_MASK
    jr _controller_toggle
_controller_toggle_caps:
    ld a, FLAGS_CAPS_MASK
_controller_toggle:
    ld hl, _editor_flags
    xor (hl)
    ld (hl), a
    ret



    ; Process the keyboard key stored in A register:
    ; Parameters:
    ;   A  - Key code to process
    ;   IX - Routine to jump to if A is a printable character
    ;   IY - Routine to jump to if A is a special ASCII character
    ; Returns:
    ;   None
    ; Alters:
    ;   A, BC, DE, HL
_editor_event_process_char:
    ; Copy character in B
    ld b, a
    call is_print
    ; C flag not set is A is printable
    ; jp nc, _controller_print_printable
    jp c, _editor_event_process_not_printable
    jp (ix)
_editor_event_process_not_printable:
    cp KB_CAPS_LOCK
    jr z, _controller_toggle_caps
    ; Check if the character is special, i.e. CTRL or SHIFT
    call controller_check_if_special
    ; Return A > 0 if it was, we can return directly then
    or a
    ; This RET will be replaced by the routines that need to receive user input without interpreting them
    ret nz
    jp (iy)

    ; Jump to this branch if the keyboard key/character to process is an ASCII
    ; special key, such as newline, backspace, arrows, etc...
    ; Parameters:
    ;   B - ASCII special character
_editor_event_process_special_ascii:
    ; Put back the character value in A
    ld a, b
    cp KB_KEY_TAB
    jr z, _controller_insert_tab
    cp KB_KEY_ENTER
    jp z, _buffer_insert_new_line
    cp KB_KEY_BACKSPACE
    jp z, _buffer_remove_char_at_cursor
    cp KB_LEFT_ARROW
    jp z, _buffer_previous_char
    cp KB_RIGHT_ARROW
    jp z, _buffer_next_char
    cp KB_UP_ARROW
    jp z, _buffer_previous_line
    cp KB_DOWN_ARROW
    jp z, _buffer_next_line
    ret

_controller_insert_tab:
    ld b, ' '
    call _buffer_insert_char_at_cursor
    ld b, ' '
    jp _buffer_insert_char_at_cursor


    ; A, B - Character to print
_controller_print_printable:
    ; If caps lock XOR shift is 1, look for the alternate keys
    ld hl, _editor_flags
    ld a, (hl)
    ; If CTRL key is pressed, interpret the key differently
    and FLAGS_CTRL_MASK
    jr nz, _controller_ctrl_combination
    ld a, (hl)
    ; Put LSB in D
    ld d, 0
    rrca
    rl d
    and 1
    xor d
    ; If the result is 0, no need to look for the alternate set
    jp z, _buffer_insert_char_at_cursor
    ; Else, we have to get the alternate set of keys
    ld a, b
    ; Check if it starts before or after 0x5B
    cp 0x5b
    jp c, _controller_print_printable_before
    ld hl, alternate_key_set_from_bracket
    sub 0x5b
    jp _controller_print_printable_table
_controller_print_printable_before:
    ld hl, alternate_key_set_from_space
    sub 0x20
_controller_print_printable_table:
    add l
    ld l, a
    adc h
    sub l
    ld h, a
    ld b, (hl)
    jp _buffer_insert_char_at_cursor


    ; A CTRL + Key combination was pressed, interpret it here
    ; Parameter:
    ;   B - Character pressed with CTRL (printable char)
    ; Returns:
    ;   -
    ; Alters:
    ;   A, BC, DE, HL
_controller_ctrl_combination:
    ld a, b
    cp KB_KEY_L
    jp z, model_show_location
    cp KB_KEY_S
    jp z, model_save_file
    cp KB_KEY_K
    jr z, _controller_ask_and_exit
    ; Unsupported feature
    ld hl, str_unsupported
    jp editor_show_notification


    ; Routine called before exiting, it will ask the user whether he wants to save the file or not
_controller_ask_and_exit:
    ; User wants to exit the program, ask whether to save the file or not
    call controller_ask_save_file
    ; If A is 0, do not save, simply exit
    ; If A is 1, save the file
    ; If A is 2, cancel
    or a
    jr z, controller_exit
    dec a
    jp z, controller_save_exit
controller_cancelled:
    ; Exit cancelled, print a message and continue
    ld hl, str_cancelled
    jp editor_show_notification
controller_save_exit:
    call model_save_file
controller_exit:
    ; Clear the screen before exiting
    ld h, DEV_STDOUT
    ld c, CMD_CLEAR_SCREEN
    IOCTL()
    EXIT()

    ; Ask the user whether he wants to save the file or not
    ; Returns:
    ;   A - 0, do not save, simply exit
    ;       1, save the file
    ;       2, cancel the operation
controller_ask_save_file:
    ld hl, str_choices
    call editor_show_notification
    ; Re-use the regular even loop to wait on Y or N or C
    ld ix, controller_ask_save_file_printable
    ld iy, controller_ask_save_file_printable_ret
    ; Empty the response
    ld a, -1
    ld (_controller_ask_response), a
    ; Call to the event loop routine
controller_ask_save_file_loop:
    call _editor_wait_for_event
    ; Check the response
    ld a, (_controller_ask_response)
    call controller_check_response
    ; Returns 0, 1, 2 or -1 if invalid
    or a
    ; If positive, return directly
    ret p
    jp controller_ask_save_file_loop


    ; Decode teh response contained in register A
    ; Parameters:
    ;   A - ASCII character corresponding to a response
    ; Returns:
    ;   A - 0 for Yes, 1 for No, 2 for Cancel, -1 else
controller_check_response:
    cp 'y'
    jr z, controller_ask_save_yes
    cp 'n'
    jr z, controller_ask_save_no
    cp 'c'
    jr z, controller_ask_save_cancel
    ld a, -1
    ret
controller_ask_save_no:
    ld a, 0
    ret
controller_ask_save_yes:
    ld a, 1
    ret
controller_ask_save_cancel:
    ld a, 2
    ret

    ; Called from the event loop!
    ; Parameters:
    ;   A, B - Keyboard key code/ASCII character
controller_ask_save_file_printable:
    ld (_controller_ask_response), a
controller_ask_save_file_printable_ret:
    ret
_controller_ask_response: DEFS 1


str_unsupported: DEFM "Not implemented", 0
str_cancelled: DEFM "Cancelled", 0
str_choices: DEFM "Save the file? Y(es) / N(o) / C(ancel)", 0
alternate_key_set_from_space:   ; Characters starting at 0x20
    DEFM " !\"#$%&\"()*+<_>?)!@#$%^&*(::<+>?"
alternate_key_set_from_bracket: ; Characters starting at 0x5B
    DEFM "{|}^_~ABCDEFGHIJKLMNOPQRSTUVWXYZ"


    SECTION DATA
_editor_keys: DEFS 32
_editor_keys_end:
    DEFS 1  ; For debugging so that symbols are correct
_editor_flags: DEFM 0


    SECTION BUFFER
    ; By adding a newline before the buffer, finding the length of the first line
    ; can use the same algorithm as the other lines
    DEFM "\n"
    ; Start of the buffer, must be the last symbol of the binary!
    PUBLIC _start_buffer
_start_buffer:

