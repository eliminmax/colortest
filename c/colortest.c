/* SPDX-FileCopyrightText: 2022-2024 Eli Array Minkoff
 *
 * SPDX-License-Identifier: GPL-3.0-only */

#include <stdio.h>
typedef unsigned char uchar;

static void color_cell(uchar n) {
    printf("\x1b[48;5;%hhum  ", n);
}

static void cube_row_part(uchar n) {
    for(uchar i = n; i < n + 6; i++) color_cell(i);
    /* use fputs instead of puts as it doesn't append a newline */
    fputs("\x1b[0m", stdout);
}

static void cube_row(uchar n) {
    cube_row_part(n);
    fputs("  ", stdout);
    cube_row_part(n + 36);
    fputs("  ", stdout);
    cube_row_part(n + 72);
    putchar('\n');
}

int main(void) {
    /* Print the first 16 colors - these vary by terminal configuration */
    putchar('\n');
    for(uchar i = 0; i < 16; i++) color_cell(i);
    /* puts appends a newline - this is desired behavior here */
    puts("\x1b[0m\n");

    /* Print the 6 sides of the color cube - these are more standardized,
     * but the order is a bit odd, thus the need for the above trickery */
    for(uchar i = 16; i < 52; i += 6) cube_row(i);
    putchar('\n');
    for(uchar i = 124; i < 160; i += 6) cube_row(i);
    putchar('\n');

    /* Finally, the 24 grays */
    /* once i wraps around to zero, it will be false.
     * (unsigned overflow is defined behavior) */
    for(uchar i = 232; i; i++) color_cell(i);
    /* once again, an extra new line should be appended here */
    puts("\x1b[0m\n");
    return 0;
}
