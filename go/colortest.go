// SPDX-FileCopyrightText: 2023 - 2025 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

package main

import "fmt"

func colorCell(n int) {
	fmt.Printf("\x1b[48;5;%dm  ", n)
}

func cubeRowPart(n int) {
	for i := n; i < n+6; i++ {
		colorCell(i)
	}
}

func cubeRow(n int) {
	cubeRowPart(n)
	fmt.Print("\x1b[0m  ")
	cubeRowPart(n + 36)
	fmt.Print("\x1b[0m  ")
	cubeRowPart(n + 72)
	fmt.Println("\x1b[0m")
}

func main() {

	// Print the first 16 colors - these vary by terminal configuration
	fmt.Println()
	for i := 0; i < 16; i++ {
		colorCell(i)
	}
	fmt.Println("\x1b[0m\n") // extra newline appended, which is intended

	// Print the 6 sides of the color cube - these are more standardized
	// but the order is a bit odd, thus the need for this trickery
	for i := 16; i < 52; i += 6 {
		cubeRow(i)
	}
	fmt.Println()
	for i := 124; i < 160; i += 6 {
		cubeRow(i)
	}
	fmt.Println()

	// Finally, the 24 grays
	for i := 232; i < 256; i++ {
		colorCell(i)
	}
	fmt.Println("\x1b[0m\n") // extra newline appended, which is intended

}

// vi:noet
