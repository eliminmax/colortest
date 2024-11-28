# SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

proc color_cell(n: uint8) = 
  stdout.write "\x1b[48;5;", n, "m  "

proc cube_row_part(n: uint8) =
  for i in (n)..(n+5):
    color_cell(i)

proc cube_row(n: uint8) =
  cube_row_part(n)
  stdout.write("\x1b[0m  ")
  cube_row_part(n + 36)
  stdout.write("\x1b[0m  ")
  cube_row_part(n + 72)
  stdout.write("\x1b[0m\n")

# Print the first 16 colors - these vary by terminal configuration
stdout.write "\n"
for i in 0'u8..15'u8:
  color_cell(i)
stdout.write "\x1b[0m\n\n"

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for i in countup(16'u8, 46'u8, 6):
  cube_row(i)
stdout.write "\n"
for i in countup(124'u8, 154'u8, 6):
  cube_row(i)
stdout.write "\n"

# Finally, the 24 grays
for i in 232'u8..255'u8:
  color_cell(i)
stdout.write "\x1b[0m\n\n"
