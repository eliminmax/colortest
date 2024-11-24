; SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
;
; SPDX-License-Identifier: GPL-3.0-only

; NASM 64-bit with Linux syscalls, no extern

; different snippets of text memory needed (index; length):
; "\x1b[0m": (0; 4)
; "\x1b[0m  ": (0; 6)
; "m  ": (3; 3)
; "  ": (4; 2)
; "\x1b[48;5;" (6; 7)
; "\n" (19, 1)
SECTION .data
    clearfmt db 0x1b,"[0m  "
    sequence_start db 0x1b,"[48;5;"
    numbuf times 3 db '0'
    newline db 0x0a
    sequence_end equ clearfmt + 3

SECTION .text
global _start

print_newline:
    MOV eax, 1
    MOV edi, 1
    MOV esi, newline
    MOV edx, 1
    SYSCALL
    RET

color_cell:
    ; print a cell with the value in rax
    PUSH rax
    MOV eax, 1
    MOV edi, 1
    MOV esi, sequence_start
    MOV edx, 7 ; length of sequence_start
    SYSCALL
    POP rax
    CALL uint8_to_ascii_str
    MOV eax, 1
    MOV edi, 1
    MOV esi, sequence_end
    MOV edx, 3
    SYSCALL
    RET

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
    CALL print_clearfmt
    MOV eax, 1
    MOV edi, 1
    MOV esi, sequence_end+1; sequence_end is "m  ", so 1 byte in is "  "
    MOV edx, 2
    SYSCALL
    RET

uint8_to_ascii_str:
    PUSH r12
    PUSH rbx
    MOV edi, numbuf
    CMP eax, 100
    JGE .three_digit
    INC edi ; skip the hundreds place
    CMP eax, 10
    JGE .two_digit
    INC edi ; skip the tens place
    MOV r12d, 1 ; it's a 1 digit number if we reach this point
    JMP .ones_place
.three_digit:
    MOV r12d, 3 ; 3 digit number
    ; because it's an u8int, we can be more efficient by only checking against 200 or 100
    CMP eax, 200
    JGE .hund200
    ; already know that it is greater than 100, so no need to check
    MOV r9b, '1' ; the ASCII code for the character
    MOV [edi], r9b ; save it to numbuf
    INC edi
    SUB eax, 100
    JMP .tens_place
.hund200:
    MOV r9b, '2' ; the ASCII code for the character
    MOV [edi], r9b
    INC edi
    SUB eax, 200
    JMP .tens_place
.two_digit:
    MOV r12d, 2
.tens_place:
    ; the DIV operation writes the quotient to eax and the remainder to edx.
    ; edx is used as the upper half of the dividend when calling it, so must be
    ; zeroed out.
    MOV ebx, 10
    XOR edx, edx
    DIV ebx
    ADD eax, '0' ; add the ASCII '0' character to the value to turn it into a digit
    MOV [edi], al
    INC edi
    ; move the remainder into eax for the ones place
    MOV eax, edx
.ones_place:
    ; this point should only be reached if the value is 9 or less
    ADD eax, '0' ; add the ASCII '0' character to the value to turn it into a digit
    MOV [edi], al
; print numbuf to stdout
    MOV eax, 1 ; syscall number for the write syscall
    MOV edi, 1 ; file descriptor for STDOUT
    MOV esi, numbuf+3 ; start right after the end of numbuf
    SUB esi, r12d ; subtract the number of characters to print
    MOV edx, r12d ; load the number of characters to print into the register
    SYSCALL ; will print the buffer contents without leading zeroes
    ; reset numbuf to "000"
    MOV edi, numbuf
    MOV eax, '0' ; the ASCII code for the character
    MOV [edi], al
    INC edi
    MOV [edi], al
    INC edi
    MOV [edi], al
    POP rbx
    POP r12
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
; but the order is a bit odd, thus the need for this trickery
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
