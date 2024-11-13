// SPDX-FileCopyrightText: 2022-2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

#include <iostream>
using namespace std;
using uchar=unsigned char;

static void color_cell(uchar n) {
    cout << "\x1b[48;5;" << +n << "m  ";
}

static void cube_row_part(uchar n) {
    for(uchar i = n; i < n + 6; i++) color_cell(i);
    cout << "\x1b[0m";
}

static void cube_row(uchar n) {
    cube_row_part(n);
    cout << "  ";
    cube_row_part(n + 36);
    cout << "  ";
    cube_row_part(n + 72);
    cout << endl;
}

int main() {
    // Print the first 16 colors - these vary by terminal configuration
    cout << endl;
    for(uchar i = 0; i < 16; i++) color_cell(i);
    // use one literal '\n' and one endl to only flush output once
    cout << "\x1b[0m\n" << endl;

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for the above trickery
    for(uchar i = 16; i < 52; i += 6) cube_row(i);
    cout << endl;
    for(uchar i = 124; i < 160; i += 6) cube_row(i);
    cout << endl;
    
    // Finally, the 24 grays
    for(short int i = 232; i < 256; i++) color_cell(static_cast<uchar>(i));
    cout << "\x1b[0m\n" << endl;

    return 0;
}
