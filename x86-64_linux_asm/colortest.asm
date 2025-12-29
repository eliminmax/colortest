; SPDX-FileCopyrightText: 2023 - 2025 Eli Array Minkoff
;
; SPDX-License-Identifier: GPL-3.0-only

; NASM 64-bit with direct Linux syscalls, and no `extern`
BITS 64

; This program first fills the buffer with the colorcell text, then performs a
; write system call, and finally an exit system call which indicates whether or
; not the write succeeded.
;
; When filling the buffer, the following registers are used as described:
; * RSI is used as the base pointer with the address of the buffer. [*] RDX is
; * used to track the offset into the buffer. [*] RAX stores the color number in
; * the 8-bit register al. RBX has constant values 100 and 10 used in division
; * instructions when
;   printing out the color cell, available in the bh and bl 8-bit registers
;   respectively.
; * RCX is used as a scratch register in the cube_row_part procedure.
;
; [*] When performing the write system call, RSI must contain the address of the
; bytes to print, and RDX must contain the number of bytes to print. I chose to
; use them as the base and offset registers so that they'd already have the
; needed values when it's time for the write syscall.

SECTION .bss
buffer: resb 4096

SECTION .text
global _start

; append the color cell to the buffer - it consists of `\e[48;5;` followed by
; the ASCII decimal representation of the value in al, then finally `m  `
color_cell:
    PUSH rax
    ; store the beginning of the escape sequence
    MOV DWORD [rsi+rdx], `\e[48`
    MOV DWORD [rsi+rdx+4], `;5;`
    ADD edx, 7
    ; check if al contains a 3-digit number
    CMP al, bh
    JAE .three_digit
    ; check if al contains a 2-digit number
    CMP al, bl
    JAE .two_digit
    JMP .one_digit
.three_digit:
    ; divide ax by 100, putting the quotient in al and the remainder in ah.
    DIV bh
    ; add the ASCII zero value to turn it into an ASCII digit
    ADD al, `0`
    ; store the result in the buffer
    MOV BYTE [rsi+rdx], al
    INC edx
    ; Because ah and al are the higher and lower 8 bits of ax, bit-shifting with
    ; the SHR instruction clears the empty bits, so shifting ax by  with SHR
    ; effectively moves the remainder from ah into ax, something that can't be
    ; done with MOV as there's no r/m16, r8 variant of the MOV instruction.
    SHR ax, 8
    ; continue into the handling of 2-digit numbers with the remainder in ax
.two_digit:
    ; divide ax by 10, putting the quotient in al and the remainder in ah.
    DIV bl
    ; add the ASCII zero value to turn it into an ASCII digit
    ADD al, `0`
    ; store the result in the buffer
    MOV BYTE [rsi+rdx], al
    INC edx
    ; perform the same technique as the hundreds digit to set ax to ah
    SHR ax, 8
    ; continue into the handling of 1-digit numbers with the remainder in ax
.one_digit:
    ; this is different - it stores the template "0m  " in memory, then adds al
    ; to the first byte of that to set the final digit.
    MOV DWORD [rsi+rdx], `0m  `
    ADD BYTE [rsi+rdx], al
    ADD edx, 4
    POP rax
    RET

cube_row_part:
    MOV cl, al
    ADD cl, 6
    .cube_row_part_loop:
        CALL color_cell
        INC al
        CMP al, cl
        JB .cube_row_part_loop
    RET

cube_row:
    CALL cube_row_part
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV WORD [rsi+rdx+4], `  `
    ADD edx, 6
    ADD al, 30
    CALL cube_row_part
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV WORD [rsi+rdx+4], `  `
    ADD edx, 6
    ADD al, 30
    CALL cube_row_part
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV BYTE [rsi+rdx+4], `\n`
    ADD edx, 5
    SUB al, 72
    RET

_start:
    ; set up the values in the RSI and RBX registers

    MOV rsi, buffer
    MOV ebx, (100 << 8) | 10 ; set bl to 10 and bh to 100

; Print the first 16 colors - these vary by terminal configuration
    ; store the leading newline in the buffer and set up RDX
    MOV BYTE [rsi], `\n`
    MOV edx, 1
    ; call color_cell for numbers 0 through 15
    XOR eax, eax ; zero out RAX more efficiently than a MOV
    .first16_loop_start:
        CALL color_cell
        INC al
        CMP al, 16
        JB .first16_loop_start
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV WORD [rsi+rdx+4], `\n\n`
    ADD edx, 6

; Print the 6 sides of the color cube - these are more standardized,
; but the order is a bit odd, thus the need for the above trickery

    ; al will already have 16 in it, so doesn't need to be explicitly set.
    .colorcube_upper_start:
        CALL cube_row
        CMP al, 52
        JB .colorcube_upper_start
    MOV BYTE [rsi+rdx], `\n`
    INC edx

    MOV al, 124
    .colorcube_lower_start:
        CALL cube_row
        CMP al, 160
        JB .colorcube_lower_start
    MOV BYTE [rsi+rdx], `\n`
    INC edx

; Finally, the 24 grays
    MOV al, 232
    .final24_loop_start:
        CALL color_cell
        INC al
        JNZ .final24_loop_start
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV WORD [rsi+rdx+4], `\n\n`
    ADD edx, 6

; write syscall
    MOV eax, 1 ; write syscall number
    MOV edi, 1 ; stdout file descriptor
    ; RSI and RDX already have address and size of the buffer, so there's no
    ; need to set them explicitly
    SYSCALL

; exit syscall
    CMP eax, edx
    ; if eax and edx compare not equal, sets dil to 1, otherwise, sets it to 0
    ; as rdi had previously held the value 1, all higher-bits are zeroed
    ; already.
    SETNE dil
    MOV eax, 60 ; exit system call number
    SYSCALL
; vim: syntax=nasm
