// SPDX-FileCopyrightText: 2022-2023 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

using System;
namespace colortest {
    class colortest {
        private const string ESC = "\x1b";
        private static void ColorCell(uint n) {
            Console.Write(ESC + "[48;5;{0}m  ", n);
        }
        private static void CubeRowPart(uint n) {
            for(uint i = n; i < n + 6; i++) {
                ColorCell(i);
            }
            Console.Write(ESC + "[0m");
        }
        private static void CubeRow(uint n) {
            CubeRowPart(n);
            Console.Write("  ");
                CubeRowPart(n + 36);
            Console.Write("  ");
            CubeRowPart(n + 72);
            Console.WriteLine();
        }
        // Print the first 16 colors - these vary by terminal configuration
        public static void Main(string[] args) {
            Console.WriteLine();
            for (uint i = 0; i < 16; i++) {
                ColorCell(i);
            }
            Console.WriteLine("\x1b[0m\n"); // Double new-line is intentional

            // Print the 6 sides of the color cube - these are more standardized
            // but the order is a bit odd, thus the need for the above trickery
            for (uint i = 16; i < 52; i+=6) {
                CubeRow(i);
            }
            Console.WriteLine();
            for (uint i = 124; i < 160; i+=6) {
                CubeRow(i);
            }
            Console.WriteLine();

            // Finally, the 24 grays
            for(uint i = 232; i < 256; i++) {
                ColorCell(i);
            }
            Console.WriteLine("\x1b[0m\n");
        }
    }
}
