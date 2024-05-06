#!/usr/bin/env elixir
# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

colorcell = fn n -> IO.write "\x1b[48;5;" <> (Integer.to_string n) <> "m  "  end
row_part_a = fn n -> Enum.map(n..(n+5), colorcell); IO.write "\x1b[0m  " end
row_part_b = fn n -> Enum.map((n+36)..(n+41), colorcell); IO.write "\x1b[0m  " end
row_part_c = fn n -> Enum.map((n+72)..(n+77), colorcell); IO.write "\x1b[0m\n" end
cube_row = fn n -> row_part_a.(n); row_part_b.(n); row_part_c.(n) end

# Print the first 16 colors - these vary by terminal configuration
IO.write "\n"
Enum.map(0..15, colorcell)
IO.write("\x1b[0m\n\n")

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for the above trickery
Enum.map(16..46//6, cube_row)
IO.write "\n"
Enum.map(124..154//6, cube_row)
IO.write "\n"

# finally, the 24 grays
Enum.map(232..255, colorcell)
IO.write("\x1b[0m\n\n")
