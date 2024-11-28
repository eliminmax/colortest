#!/usr/bin/env ruby

# SPDX-FileCopyrightText: 2022 - 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

def color_cells(colors)
  colors.map{|n| ("\x1b[48;5;%dm  " % [n])}.join() + "\x1b[0m"
end

def cube_row(n)
  return [n, n + 36, n + 72].map{|i| color_cells((i...(i+6)))}.join("  ")
end

# Print the first 16 colors - these vary by terminal configuration
puts
puts color_cells((0...16))
puts

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for i in (16...52).step(6)
  puts cube_row(i)
end
puts
for i in (124...160).step(6)
  puts cube_row(i)
end
puts

# Finally, the 24 grays
puts color_cells((232...256))
puts
