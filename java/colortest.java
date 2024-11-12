// SPDX-FileCopyrightText: 2022-2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

public class colortest {
    private static void colorCell(short n) {
        System.out.printf("\033[48;5;%dm  ", n);
    }
    private static void cubeRowPart(short n) {
        for (short i = n; i < n + 6; i++) colorCell(i);
        System.out.print("\033[0m");
    }
    private static void cubeRow(short n) {
        cubeRowPart(n);
        System.out.print("  ");
        cubeRowPart((short)(n + 36));
        System.out.print("  ");
        cubeRowPart((short)(n + 72));
        System.out.println();
    }
    public static void main(String[] args) {
        // Print the first 16 colors - these vary by terminal configuration
        System.out.println();
        for (short i = 0; i < 16; i++) colorCell(i);
        System.out.println("\033[0m\n"); // Double new-line is intentional

        // Print the 6 sides of the color cube - these are more standardized
        // but the order is a bit odd, thus the need for this trickery
        for(short i = 16; i < 52; i += 6) cubeRow(i);
        System.out.println();
        for(short i = 124; i < 160; i += 6) cubeRow(i);
        System.out.println();

        // Finally, the 24 grays
        for(short i = 232; i < 256; i++) colorCell(i);
        System.out.println("\033[0m\n");
    }
}
