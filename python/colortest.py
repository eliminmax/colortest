#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2022-2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

from collections.abc import Iterable


def color_cells(nums: Iterable[int]) -> str:
    return ''.join(f"\x1b[48;5;{i}m  " for i in nums) + "\x1b[0m"


def cube_row(n: int):
    print(color_cells(range(n, n+6)), end="  ")
    print(color_cells(range(n+36, n+42)), end="  ")
    print(color_cells(range(n+72, n+78)))


# Print the first 16 colors - these vary by terminal configuration
print()
print(color_cells(range(16)), end="\n\n")

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for the above trickery
for i in range(16, 52, 6):
    cube_row(i)
print()
for i in range(124, 160, 6):
    cube_row(i)
print()

# Finally, the 24 grays
print(color_cells(range(232, 256)), end="\n\n")
