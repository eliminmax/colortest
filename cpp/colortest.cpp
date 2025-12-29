// SPDX-FileCopyrightText: 2022 - 2025 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

#include <iostream>
using namespace std;

static void color_cell(int n) {
    cout << "\x1b[48;5;" << n << "m  ";
}

static void cube_row_part(int n) {
    for (int i = n; i < n + 6; i++) color_cell(i);
}

static void cube_row(int n) {
    cube_row_part(n);
    cout << "\x1b[0m  ";
    cube_row_part(n + 36);
    cout << "\x1b[0m  ";
    cube_row_part(n + 72);
    cout << "\x1b[0m\n";
}

int main() {
    // Print the first 16 colors - these vary by terminal configuration
    cout << "\n";
    for (int i = 0; i < 16; i++) color_cell(i);
    cout << "\x1b[0m\n\n";

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for the above trickery
    for (int i = 16; i < 52; i += 6) cube_row(i);
    cout << '\n';
    for (int i = 124; i < 160; i += 6) cube_row(i);
    cout << '\n';

    // Finally, the 24 grays
    for (int i = 232; i < 256; i++) color_cell(i);
    cout << "\x1b[0m\n\n";

    return 0;
}
