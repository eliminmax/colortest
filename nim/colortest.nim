# SPDX-FileCopyrightText: 2023 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# Print the first 16 colors - these vary by terminal configuration
stdout.write "\n"
for i in 0..15:
  stdout.write "\x1b[48;5;", i, "m  "
stdout.write "\x1b[0m\n\n"

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for i in countup(16, 46, 6):
  for ii in 0..5:
    stdout.write "\x1b[48;5;", i+ii, "m  "
  stdout.write "\x1b[0m  "
  for ii in 36..41:
    stdout.write "\x1b[48;5;", i+ii, "m  "
  stdout.write "\x1b[0m  "
  for ii in 72..77:
    stdout.write "\x1b[48;5;", i+ii, "m  "
  stdout.write "\x1b[0m\n"
stdout.write "\n"
for i in countup(124, 154, 6):
  for ii in 0..5:
    stdout.write "\x1b[48;5;", i+ii, "m  "
  stdout.write "\x1b[0m  "
  for ii in 36..41:
    stdout.write "\x1b[48;5;", i+ii, "m  "
  stdout.write "\x1b[0m  "
  for ii in 72..77:
    stdout.write "\x1b[48;5;", i+ii, "m  "
  stdout.write "\x1b[0m\n"
stdout.write "\n"

# Finally, the 24 grays
for i in 232..255:
  stdout.write "\x1b[48;5;", i, "m  "
stdout.write "\x1b[0m\n\n"
