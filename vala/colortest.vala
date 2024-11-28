// SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

void color_cell (uint8 n) {
    stdout.printf("\x1b[48;5;%dm  ", n);
}
void cube_row_part(uint8 n) {
    for (uint8 i = n; i < n + 6; i++) color_cell(i);
}

void cube_row (uint8 n) {
    cube_row_part(n);
    stdout.printf("\x1b[0m  ");
    cube_row_part(n + 36);
    stdout.printf("\x1b[0m  ");
    cube_row_part(n + 72);
    stdout.printf("\x1b[0m\n");
}

void main () {
    // Print the first 16 colors - these vary by terminal configuration
    stdout.printf ("\n");
    for (uint8 i = 0; i < 16; i++) color_cell(i);
    stdout.printf("\x1b[0m\n\n");

    // Print the 6 sides of the color cube - these are more standardized,
    // but the order is a bit odd, thus the need for this trickery
    for (uint8 i = 16; i < 52; i+=6) cube_row(i);
    stdout.printf("\n");
    for (uint8 i = 124; i < 160; i+=6) cube_row(i);
    stdout.printf("\n");

    // Finally, the 24 grays
    // loop iterator offset by 128 to avoid overflow
    for (uint8 i = 104; i < 128; i++) {
        stdout.printf ("\x1b[48;5;%dm  ", i | 128);
    }
    stdout.printf("\x1b[0m\n\n");
}
