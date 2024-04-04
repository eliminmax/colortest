// SPDX-FileCopyrightText: 2023 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

package main

import "fmt"

func main() {
	// Print the first 16 colors - these vary by terminal configuration
	fmt.Println()
	for i := 0; i < 16; i++ {
		fmt.Printf("\x1b[48;5;%dm  ", i)
	}
	fmt.Println("\x1b[0m\n")

	// Print the 6 sides of the color cube - these are more standardized
	// but the order is a bit odd, thus the need for this trickery
	for i := 16; i < 52; i += 6 {
		for ii := 0; ii < 6; ii++ {
			fmt.Printf("\x1b[48;5;%dm  ", i+ii)
		}
		fmt.Print("\x1b[0m  ")
		for ii := 36; ii < 42; ii++ {
			fmt.Printf("\x1b[48;5;%dm  ", i+ii)
		}
		fmt.Print("\x1b[0m  ")
		for ii := 72; ii < 78; ii++ {
			fmt.Printf("\x1b[48;5;%dm  ", i+ii)
		}
		fmt.Println("\x1b[0m")
	}
	fmt.Println()
	for i := 124; i < 160; i += 6 {
		for ii := 0; ii < 6; ii++ {
			fmt.Printf("\x1b[48;5;%dm  ", i+ii)
		}
		fmt.Print("\x1b[0m  ")
		for ii := 36; ii < 42; ii++ {
			fmt.Printf("\x1b[48;5;%dm  ", i+ii)
		}
		fmt.Print("\x1b[0m  ")
		for ii := 72; ii < 78; ii++ {
			fmt.Printf("\x1b[48;5;%dm  ", i+ii)
		}
		fmt.Println("\x1b[0m")
	}
	fmt.Println()

	// Finally, the 24 grays
	for i := 232; i < 256; i++ {
		fmt.Printf("\x1b[48;5;%dm  ", i)
	}
	fmt.Println("\x1b[0m\n")

}

// vi:noet
