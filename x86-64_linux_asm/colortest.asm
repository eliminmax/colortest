; SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
;
; SPDX-License-Identifier: GPL-3.0-only

; NASM 64-bit with Linux syscalls, no extern

SECTION .data
    newline db 0x0a
    clearfmt db 0x1b,"[0m  "
    ; numbuf is the part of color_cell_text that contains the ASCII
    ; representation of the cell number, followed by "m  ".
    color_cell_text db 0x1b,"[48;5;"
    numbuf times 6 db 0

SECTION .text
global _start

print_newline:
    MOV edx, 1
    MOV eax, 1
    MOV edi, 1
    MOV esi, newline
    SYSCALL
    RET

    ; set numbuf to the ASCII representation of EAX followed by "m  ", then
    ; call the "write" syscall on color_cell_text, with a length long enough
    ; to print the 
color_cell:
    MOV edi, numbuf
    CMP eax, 100
    JGE .three_digit
    CMP eax, 10
    JGE .two_digit
    MOV edx, 11; length of a color cell with 1-digit number
.ones_place:
    ADD al, '0'
    MOV [edi], al
    ; set the 3 bytes after the number text to "m  "
    MOV BYTE [edi+1], 'm'
    MOV BYTE [edi+2], 0x20; ASCII space
    MOV BYTE [edi+3], 0x20
    MOV eax, 1 ; the WRITE system call number
    MOV edi, 1 ; file descriptor #1 (stdout)
    MOV esi, color_cell_text
    ; edx is already set to the number of bytes to print.
    SYSCALL
    RET
.three_digit:
    MOV edx, 13; length of a color cell with 3-digit number
    MOV BYTE [edi], '1'
    INC edi
    SUB eax, 100 ; first subtract 100
    CMP eax, 100 ; if still above 100, then the leading digit should be 2
    JGE .hund200
    JMP .tens_place
.hund200:
    INC BYTE [edi-1] ; increment the '1' character so that it becomes '2'
    SUB eax, 100; subtract another 100 to make it a 2 digit number
    JMP .tens_place
.two_digit:
    MOV edx, 12; length of a color cell with 1-digit number
.tens_place:
    ; the DIV operation writes the quotient to eax and the remainder to edx.
    ; edx is used as the upper half of the dividend when calling it, so must be
    ; zeroed out.
    MOV ecx, 10
    PUSH rdx ; push the lenth set at the start of the function
    XOR edx, edx
    DIV ecx
    ADD eax, '0' ; add the ASCII '0' character to the value to turn it into a digit
    MOV [edi], al
    INC edi
    ; move the remainder into eax for the ones place
    MOV eax, edx
    POP rdx ; restore the length
    JMP .ones_place

cube_row_part:
    PUSH rbx
    PUSH rbp
    MOV ebp, eax
    MOV ebx, eax
    ADD ebx, 6
    .cube_row_part_loop:
    MOV eax, ebp
    CALL color_cell
    INC ebp
    CMP ebp, ebx
    JL .cube_row_part_loop
    POP rbp
    POP rbx
    RET

cube_row:
    PUSH rax
    CALL cube_row_part
    CALL print_blank_cell
    POP rax
    ADD eax, 36
    PUSH rax
    CALL cube_row_part
    CALL print_blank_cell
    POP rax
    ADD eax, 36
    CALL cube_row_part
    CALL print_clearfmt
    CALL print_newline
    RET

print_clearfmt:
    MOV eax, 1
    MOV edi, 1
    MOV esi, clearfmt
    MOV edx, 4
    SYSCALL
    RET

print_blank_cell:
    MOV eax, 1
    MOV edi, 1
    MOV esi, clearfmt
    MOV edx, 6; 2 spaces are right after the end of the sequence
    SYSCALL
    RET

_start:
; Print the first 16 colors - these vary by terminal configuration
    CALL print_newline
    XOR ebx, ebx
    .first16_loop_start:
    MOV eax, ebx
    CALL color_cell
    INC ebx
    CMP ebx, 16
    JL .first16_loop_start
    CALL print_clearfmt
    CALL print_newline
    CALL print_newline

; Print the 6 sides of the color cube - these are more standardized,
; but the order is a bit odd, thus the need for the above trickery
    MOV ebx, 16
    .colorcube_upper_start:
        MOV eax, ebx
        CALL cube_row
        ADD ebx, 6
        CMP ebx, 52
    JL .colorcube_upper_start
    CALL print_newline
    MOV ebx, 124
    .colorcube_lower_start:
        MOV eax, ebx
        CALL cube_row
        ADD ebx, 6
        CMP ebx, 160
    JL .colorcube_lower_start
    CALL print_newline

; Finally, the 24 grays
MOV ebx, 232
.final24_loop_start:
    MOV eax, ebx
    CALL color_cell
    INC ebx
    CMP ebx, 256
    JL .final24_loop_start
    CALL print_clearfmt
    CALL print_newline
    CALL print_newline

; exit syscall
    MOV eax, 60 ; the EXIT system call number
    XOR edi, edi ; sets edi to 0 more efficiently than MOV
    SYSCALL
