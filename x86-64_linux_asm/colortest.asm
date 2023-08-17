; NASM 64-bit with Linux syscalls, no extern

SECTION .data
    newline db 0x0a
    blank_cell db "  "
    clearfmt db 0x1b,"[0m"
    sequence_start db 0x1b,"[48;5;"
    sequence_end db "m  "
    numbuf db "000"

SECTION .text
global _start

print_newline:
    MOV eax, 1
    MOV edi, 1
    MOV esi, newline
    MOV edx, 1
    SYSCALL
    RET

print_cell:
    ; print a cell with the value in eax
    PUSH rax
    MOV eax, 1
    MOV edi, 1
    MOV esi, sequence_start
    MOV edx, 7 ; lenght of sequence_start
    SYSCALL
    POP rax
    CALL uint8_to_ascii_str
    MOV eax, 1
    MOV edi, 1
    MOV esi, sequence_end
    MOV edx, 3
    SYSCALL
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
    MOV esi, blank_cell
    MOV edx, 2
    SYSCALL
    RET

uint8_to_ascii_str:
    MOV edi, numbuf
    CMP eax, 100
    JGE three_digit
    INC edi ; skip the hundreds place
    CMP eax, 10
    JGE two_digit
    INC edi ; skip the tens place
    MOV r12d, 1 ; it's a 1 digit number if we reach this point
    JMP ones_place
three_digit:
    MOV r12d, 3 ; 3 digit number
    ; because it's an u8int, we can be more efficient by only checking against 200 or 100
    CMP eax, 200
    JGE hund200
    ; already know that it is greater than 100, so no need to check
    MOV r9b, 0x31 ; ASCII value of '1'
    MOV [edi], r9b ; save it to numbuf
    INC edi
    SUB eax, 100
    JMP tens_place
hund200:
    MOV r9b, 0x32 ; ASCII value of '2'
    MOV [edi], r9b
    INC edi
    SUB eax, 200
    JMP tens_place
two_digit:
    MOV r12d, 2
tens_place:
; the DIV operation writes the quotient to eax and the remainder to edx.
; To convert a base-10 int that is less than 10 to its ASCII symbol, add 0x30 to its value
    XOR edx, edx
    DIV ebx
    ADD eax, 0x30
    MOV [edi], al
    INC edi
    ; move the remainder into eax for the ones place
    MOV eax, edx
ones_place:
    ; this point should only be reached if the value is 9 or less
    ADD eax, 0x30
    MOV [edi], al
; print numbuf to stdout
    MOV eax, 1 ; syscall number for the write syscall
    MOV edi, 1 ; file descriptor for STDOUT
    MOV esi, numbuf+3 ; start right after the end of numbuf
    SUB esi, r12d ; subtract the number of characters to print
    MOV edx, r12d ; load the number of characters to print into the register
    SYSCALL ; will print the buffer contents without leading zeroes
    ; reset edi
    MOV edi, numbuf
    MOV eax, 0x30
    MOV [edi], al
    INC edi
    MOV [edi], al
    INC edi
    MOV [edi], al
    RET

_start:
    MOV ebx, 10 ; used for DIV operation

; Print the first 16 colors - these vary by terminal configuration
    CALL print_newline
    MOV ecx, 0
first16_loop_start:
    PUSH rcx
    MOV eax, ecx
    CALL print_cell
    POP rcx
    INC ecx
    CMP ecx, 16
    JL first16_loop_start
    CALL print_clearfmt
    CALL print_newline
    CALL print_newline

; Print the 6 sides of the color cube - these are more standardized,
; but the order is a bit odd, thus the need for this trickery
    MOV ecx, 16
colorcube_upper_start:
    XOR r8, r8 ; sets r8 to 0 more efficiently than MOV
    colorcube_upper_row_a_start:
        MOV eax, ecx
        ADD eax, r8d
        PUSH rcx
        CALL print_cell
        POP rcx
        INC r8
        CMP r8, 6
        JL colorcube_upper_row_a_start
    PUSH rcx
    CALL print_blank_cell
    POP rcx
    MOV r8d, 36
    colorcube_upper_row_b_start:
        MOV eax, ecx
        ADD eax, r8d
        PUSH rcx
        CALL print_cell
        POP rcx
        INC r8
        CMP r8, 42
        JL colorcube_upper_row_b_start
    PUSH rcx
    CALL print_blank_cell
    POP rcx
    MOV r8d, 72
    colorcube_upper_row_c_start:
        MOV eax, ecx
        ADD eax, r8d
        PUSH rcx
        CALL print_cell
        POP rcx
        INC r8
        CMP r8, 78
        JL colorcube_upper_row_c_start
    PUSH rcx
    CALL print_clearfmt
    CALL print_newline
    POP rcx
    ADD rcx, 6
    CMP rcx, 52
    JL colorcube_upper_start
    CALL print_newline
    MOV ecx, 124
colorcube_lower_start:
    XOR r8, r8 ; sets r8 to 0 more efficiently than MOV
    colorcube_lower_row_a_start:
        MOV eax, ecx
        ADD eax, r8d
        PUSH rcx
        CALL print_cell
        POP rcx
        INC r8
        CMP r8, 6
        JL colorcube_lower_row_a_start
    PUSH rcx
    CALL print_blank_cell
    POP rcx
    MOV r8d, 36
    colorcube_lower_row_b_start:
        MOV eax, ecx
        ADD eax, r8d
        PUSH rcx
        CALL print_cell
        POP rcx
        INC r8
        CMP r8, 42
        JL colorcube_lower_row_b_start
    PUSH rcx
    CALL print_blank_cell
    POP rcx
    MOV r8d, 72
    colorcube_lower_row_c_start:
        MOV eax, ecx
        ADD eax, r8d
        PUSH rcx
        CALL print_cell
        POP rcx
        INC r8
        CMP r8, 78
        JL colorcube_lower_row_c_start
    PUSH rcx
    CALL print_clearfmt
    CALL print_newline
    POP rcx
    ADD rcx, 6
    CMP rcx, 160
    JL colorcube_lower_start
    CALL print_newline

; Finally, the 24 grays
MOV ecx, 232
final24_loop_start:
    PUSH rcx
    MOV eax, ecx
    CALL print_cell
    POP rcx
    INC ecx
    CMP ecx, 256
    JL final24_loop_start
    CALL print_clearfmt
    CALL print_newline
    CALL print_newline


; exit syscall
    MOV eax, 60
    MOV edi, 0
    SYSCALL
