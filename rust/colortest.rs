// SPDX-FileCopyrightText: 2022-2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

fn color_cell(n: u8) {
    print!("\x1b[48;5;{n}m  ");
}

fn cube_row(n: u8) {
    (n..(n + 6)).for_each(color_cell);
    print!("\x1b[0m  ");
    ((n + 36)..(n + 42)).for_each(color_cell);
    print!("\x1b[0m  ");
    ((n + 72)..(n + 78)).for_each(color_cell);
    println!("\x1b[0m");
}

fn main() {
    // Print the first 16 colors - these vary by terminal configuration
    println!();
    (0u8..16).for_each(color_cell);
    println!("\x1b[0m\n");

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for this trickery
    (16u8..52).step_by(6).for_each(cube_row);
    println!();
    (124u8..160).step_by(6).for_each(cube_row);
    println!();

    // Finally, the 24 grays
    (232u8..=255).for_each(color_cell);
    println!("\x1b[0m\n")
}
