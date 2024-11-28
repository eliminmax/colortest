// SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

import std.stdio;

void color_cell(ubyte n) {
    write("\x1b[48;5;", n, "m  ");
}

void cube_row_part(ubyte n) {
    for(ubyte i = n; i < n + 6; i++) color_cell(i);
}

void cube_row(ubyte n) {
    cube_row_part(n);
    write("\x1b[0m  ");
    // Calling cube_row_part(n + 36) fails because n is implicitly cast to int
    // Adding to n in a separate line does not implicitly cast.
    n += 36;
    cube_row_part(n);
    write("\x1b[0m  ");
    // Now add 36 again (normally would call cube_row_part(n + 72))
    n += 36;
    cube_row_part(n);
    write("\x1b[0m\n");
}

void main() {
    // Print the first 16 colors - these vary by terminal configuration
    writeln();
    for(ubyte i = 0; i < 16; i++) color_cell(i);
    writeln("\x1b[0m\n"); // extra newline appended, which is intended

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for the above trickery
    for(ubyte i = 16; i < 52; i += 6) cube_row(i);
    writeln();
    for(ubyte i = 124; i < 160; i += 6) cube_row(i);
    writeln();

    // Finally, the 24 grays
    // once i wraps around to zero, it will be false.
    for(ubyte i = 232; i; i++) color_cell(i);
    writeln("\x1b[0m\n");
}
