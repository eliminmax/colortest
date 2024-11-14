// SPDX-FileCopyrightText: 2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

package main

import "core:fmt"

color_cell :: proc(n: int) {
    fmt.printf("\e[48;5;%dm  ", n)
}

cube_row_part :: proc(n: int) {
    for i in n..<(n+6) {
        color_cell(i)
    }
}

cube_row :: proc(n: int) {
    cube_row_part(n)
    fmt.print("\e[0m  ")
    cube_row_part(n + 36)
    fmt.print("\e[0m  ")
    cube_row_part(n + 72)
    fmt.print("\e[0m\n")
}

main :: proc() {
    // Print the first 16 colors - these vary by terminal configuration
	fmt.println("")
    for i in 0..<16 {
        fmt.printf("\e[48;5;%dm  ", i)       
    }
    fmt.println("\e[0m\n")

    // Print the 6 sides of the color cube - these are more standardized,
    // but the order is a bit odd, thus the need for this trickery
    for i := 16; i < 52; i += 6 {
        cube_row(i)
    }
    fmt.println("")
    for i := 124; i < 160; i += 6 {
        cube_row(i)
    }
    fmt.println("")

    // Finally, the 24 grays
    for i in 232..<256 {
        color_cell(i)
    }
    fmt.println("\e[0m\n")
}
