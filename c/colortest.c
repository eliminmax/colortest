/* SPDX-FileCopyrightText: 2022 - 2025 Eli Array Minkoff
 *
 * SPDX-License-Identifier: GPL-3.0-only */

#include <limits.h>
#include <stdio.h>
typedef unsigned char uchar;

static void color_cell(uchar n) {
    printf("\x1b[48;5;%hhum  ", n);
}

static void cube_row_part(uchar n) {
    for (uchar i = n; i < n + 6; i++) color_cell(i);
}

static void cube_row(uchar n) {
    cube_row_part(n);
    /* use fputs instead of puts as it doesn't append a newline */
    fputs("\x1b[0m  ", stdout);
    cube_row_part(n + 36);
    fputs("\x1b[0m  ", stdout);
    cube_row_part(n + 72);
    /* this time, a newline is desired */
    puts("\x1b[0m");
}

int main(void) {
    /* Print the first 16 colors - these vary by terminal configuration */
    putchar('\n');
    for (uchar i = 0; i < 16; i++) color_cell(i);
    /* puts appends a newline - this is desired behavior here */
    puts("\x1b[0m\n");

    /* Print the 6 sides of the color cube - these are more standardized,
     * but the order is a bit odd, thus the need for the above trickery */
    for (uchar i = 16; i < 52; i += 6) cube_row(i);
    putchar('\n');
    for (uchar i = 124; i < 160; i += 6) cube_row(i);
    putchar('\n');

    /* Finally, the 24 grays */
    for (unsigned i = 232; i < 256; i++) color_cell(i);
    /* once again, an extra new line should be appended here */
    puts("\x1b[0m\n");
    return 0;
}
