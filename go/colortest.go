// SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

package main

import "fmt"

func colorCell(n uint8) {
	fmt.Printf("\x1b[48;5;%dm  ", n)
}

func cubeRowPart(n uint8) {
	for i := n; i < n + 6; i++ {
		colorCell(i)
	}
	fmt.Print("\x1b[0m")
}

func cubeRow(n uint8) {
	cubeRowPart(n)
	fmt.Print("  ")
	cubeRowPart(n + 36)
	fmt.Print("  ")
	cubeRowPart(n + 72)
	fmt.Println()
}

func main() {
	// Print the first 16 colors - these vary by terminal configuration
	fmt.Println()
	for i := uint8(0); i < 16; i++ {
		colorCell(i)
	}
	fmt.Println("\x1b[0m\n") // extra newline appended, which is intended

	// Print the 6 sides of the color cube - these are more standardized
	// but the order is a bit odd, thus the need for this trickery
	for i := uint8(16); i < 52; i += 6 {
		cubeRow(i)
	}
	fmt.Println()
	for i := uint8(124); i < 160; i += 6 {
		cubeRow(i)
	}
	fmt.Println()

	// Finally, the 24 grays
	// i != 0 is used because after the last cell, i overflows, and Go wraps on
	// unsigned overflow.
	for i := uint8(232); i != 0; i++ {
		colorCell(i)
	}
	fmt.Println("\x1b[0m\n") // extra newline appended, which is intended

}

// vi:noet
