#!/usr/bin/env elixir
# SPDX-FileCopyrightText: 2024 - 2025 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

color_cell = fn n -> IO.write "\x1b[48;5;" <> (Integer.to_string n) <> "m  "  end
cube_row_part = fn n -> Enum.map(n..(n+5), color_cell); end
cube_row = fn n ->
  cube_row_part.(n);    IO.write "\x1b[0m  "
  cube_row_part.(n+36); IO.write "\x1b[0m  "
  cube_row_part.(n+72); IO.write "\x1b[0m\n"
end

# Print the first 16 colors - these vary by terminal configuration
IO.write "\n"
Enum.map(0..15, color_cell)
IO.write "\x1b[0m\n\n"

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for the above trickery
Enum.map(16..46//6, cube_row)
IO.write "\n"
Enum.map(124..154//6, cube_row)
IO.write "\n"

# finally, the 24 grays
Enum.map(232..255, color_cell)
IO.write "\x1b[0m\n\n"
