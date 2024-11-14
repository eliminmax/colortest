#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2022-2023 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only


def color_cell(n: int):
    print(f"\x1b[48;5;{n}m  ", end="")


def cube_row_part(n: int):
    for i in range(n, n+6):
        color_cell(i)


def cube_row(n: int):
    cube_row_part(n)
    print("\x1b[0m  ", end="")
    cube_row_part(n + 36)
    print("\x1b[0m  ", end="")
    cube_row_part(n + 72)
    print("\x1b[0m")


# Print the first 16 colors - these vary by terminal configuration
print()
for i in range(16):
    color_cell(i)
print("\x1b[0m\n")

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for the above trickery
for i in range(16, 52, 6):
    cube_row(i)
print()
for i in range(124, 160, 6):
    cube_row(i)
print()

# Finally, the 24 grays
for i in range(232, 256):
    color_cell(i)
print("\x1b[0m\n")
