// SPDX-FileCopyrightText: 2022 - 2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

public class colortest {
    private static void colorCell(int n) {
        System.out.printf("\033[48;5;%dm  ", n);
    }
    private static void cubeRowPart(int n) {
        for (int i = n; i < n + 6; i++) colorCell(i);
    }
    private static void cubeRow(int n) {
        cubeRowPart(n);
        System.out.print("\033[0m  ");
        cubeRowPart(n + 36);
        System.out.print("\033[0m  ");
        cubeRowPart(n + 72);
        System.out.println("\033[0m");
    }
    public static void main(String[] args) {
        // Print the first 16 colors - these vary by terminal configuration
        System.out.println();
        for (int i = 0; i < 16; i++) colorCell(i);
        System.out.println("\033[0m\n"); // Double new-line is intentional

        // Print the 6 sides of the color cube - these are more standardized
        // but the order is a bit odd, thus the need for this trickery
        for(int i = 16; i < 52; i += 6) cubeRow(i);
        System.out.println();
        for(int i = 124; i < 160; i += 6) cubeRow(i);
        System.out.println();

        // Finally, the 24 grays
        for(int i = 232; i < 256; i++) colorCell(i);
        System.out.println("\033[0m\n");
    }
}
