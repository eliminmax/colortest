// SPDX-FileCopyrightText: 2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

package main

import "core:fmt"

main :: proc() {
    // Print the first 16 colors - these vary by terminal configuration
	fmt.println("")
    for i := 0; i < 16; i += 1 {
        fmt.printf("\e[48;5;%dm  ", i)       
    }
    fmt.println("\e[0m\n")

    // Print the 6 sides of the color cube - these are more standardized,
    // but the order is a bit odd, thus the need for this trickery
    for i := 16; i < 52; i += 6 {
        for ii := 0; ii < 6; ii += 1 {
            fmt.printf("\e[48;5;%dm  ", i + ii)
        }
        fmt.print("\e[0m  ")
        for ii := 36; ii < 42; ii += 1 {
            fmt.printf("\e[48;5;%dm  ", i + ii)
        }
        fmt.print("\e[0m  ")
        for ii := 72; ii < 78; ii += 1 {
            fmt.printf("\e[48;5;%dm  ", i + ii)
        }
        fmt.println("\e[0m")
    }
    fmt.println("")
    for i := 124; i < 160; i += 6 {
        for ii := 0; ii < 6; ii += 1 {
            fmt.printf("\e[48;5;%dm  ", i + ii)
        }
        fmt.print("\e[0m  ")
        for ii := 36; ii < 42; ii += 1 {
            fmt.printf("\e[48;5;%dm  ", i + ii)
        }
        fmt.print("\e[0m  ")
        for ii := 72; ii < 78; ii += 1 {
            fmt.printf("\e[48;5;%dm  ", i + ii)
        }
        fmt.println("\e[0m")
    }
    fmt.println("")

    // Finally, the 24 grays
    for i := 232; i < 256; i += 1 {
        fmt.printf("\e[48;5;%dm  ", i)
    }
    fmt.println("\e[0m\n")
}
