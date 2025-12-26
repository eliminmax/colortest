; SPDX-FileCopyrightText: 2023 - 2025 Eli Array Minkoff
;
; SPDX-License-Identifier: GPL-3.0-only

; NASM 64-bit with Linux syscalls, no extern
BITS 64

SECTION .bss
    buffer: resb 4096

SECTION .text
global _start

    ; append the color cell to the buffer - it consists of `\e[48;5;` followed by
    ; the ASCII decimal representation of the value `al` (the lowest 8 bits of
    ; RAX), then finally `m  `
color_cell:
    PUSH rax
    MOV DWORD [rsi+rdx], `\x1b[48`
    MOV DWORD [rsi+rdx+4], `;5;`
    ADD rdx, 7
    MOV bx, (100 << 8) | 10
    CMP al, bh
    JAE .three_digit
    CMP al, bl
    JAE .two_digit
    JMP .one_digit
.three_digit:
    DIV bh
    ADD al, `0`
    MOV BYTE [rsi+rdx], al
    INC rdx
    SHR ax, 8
.two_digit:
    DIV bl
    ADD al, `0`
    MOV BYTE [rsi+rdx], al
    INC rdx
    SHR ax, 8
.one_digit:
    MOV DWORD [rsi+rdx], `0m  `
    ADD BYTE [rsi+rdx], al
    ADD rdx, 4
    POP rax
    RET

cube_row_part:
    MOV cl, al
    ADD cl, 6
    REP 
    .cube_row_part_loop:
    CALL color_cell
    INC al
    CMP al, cl
    JB .cube_row_part_loop
    RET

cube_row:
    PUSH rax
    CALL cube_row_part
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV WORD [rsi+rdx+4], `  `
    ADD rdx, 6
    ADD al, 30
    CALL cube_row_part
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV WORD [rsi+rdx+4], `  `
    ADD rdx, 6
    ADD al, 30
    CALL cube_row_part
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV BYTE [rsi+rdx+4], `\n`
    ADD rdx, 5
    POP rax
    RET

_start:
    MOV rsi, buffer
    MOV Byte [rsi], `\n`
    MOV rdx, 1
    XOR eax, eax
; Print the first 16 colors - these vary by terminal configuration
    .first16_loop_start:
    CALL color_cell
    INC al
    CMP al, 16
    JB .first16_loop_start
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV WORD [rsi+rdx+4], `\n\n`
    ADD rdx, 6

; Print the 6 sides of the color cube - these are more standardized,
; but the order is a bit odd, thus the need for the above trickery
    MOV al, 16
    .colorcube_upper_start:
        CALL cube_row
        ADD al, 6
        CMP al, 52
    JB .colorcube_upper_start
    MOV BYTE [rsi+rdx], `\n`
    INC rdx

    MOV al, 124
    .colorcube_lower_start:
        CALL cube_row
        ADD al, 6
        CMP al, 160
    JB .colorcube_lower_start
    MOV BYTE [rsi+rdx], `\n`
    INC rdx

    MOV ax, 232
; Finally, the 24 grays
.final24_loop_start:
    CALL color_cell
    INC ax
    CMP ax, 256
    JB .final24_loop_start
    MOV DWORD [rsi+rdx], `\e[0m`
    MOV WORD [rsi+rdx+4], `\n\n`
    ADD rdx, 6

; write syscall
    MOV eax, 1 ; write syscall number
    MOV edi, 1 ; stdout file descriptor
    SYSCALL

; exit syscall
    XOR edi, edi ; sets edi to 0 more efficiently than MOV
    CMP eax, buffer
    JB .end
    INC edi
.end:
    MOV eax, 60 ; exit system call number
    SYSCALL
