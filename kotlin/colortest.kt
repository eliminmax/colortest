// SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

fun color_cell(n: Int) {
    print("\u001b[48;5;${n}m  ")
}

fun cube_row_part(n: Int) {
    for (i in n..(n+5)) color_cell(i)
    print("\u001b[0m")
}

fun cube_row(n: Int) {
    cube_row_part(n)
    print("  ")
    cube_row_part(n + 36)
    print("  ")
    cube_row_part(n + 72)
    println()
}

fun main() {
    // Print the first 16 colors - these vary by terminal configuration
    println()
    for (i in 0..15) color_cell(i)
    println("\u001b[0m\n")

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for this trickery
    for (i in 16..46 step 6) cube_row(i)
    println()
    for (i in 124..154 step 6) cube_row(i)
    println()

    // Finally, the 24 grays
    for (i in 232..255) color_cell(i)
    println("\u001b[0m\n")
}
