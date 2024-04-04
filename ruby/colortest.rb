#!/usr/bin/env ruby

# SPDX-FileCopyrightText: 2022 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# Print the first 16 colors - these vary by terminal configuration
puts ""
for i in 0..15
  print "\x1b[48;5;#{i}m  "
end
puts "\x1b[0m"
puts

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for i in (16...52).step(6)
  row_a = []
  row_b = []
  row_c = []
  for ii in 0...6
    row_a.push(i+ii)
  end
  for ii in 36...42
    row_b.push(i+ii)
  end
  for ii in 72...78
    row_c.push(i+ii)
  end
  for ii in row_a
    print "\x1b[48;5;#{ii}m  "
  end
  print "\x1b[0m  "
  for ii in row_b
    print "\x1b[48;5;#{ii}m  "
  end
  print "\x1b[0m  "
  for ii in row_c
    print "\x1b[48;5;#{ii}m  "
  end
  puts "\x1b[0m"
end
puts
for i in (124...160).step(6)
  row_a = []
  row_b = []
  row_c = []
  for ii in 0...6
    row_a.push(i+ii)
  end
  for ii in 36...42
    row_b.push(i+ii)
  end
  for ii in 72...78
    row_c.push(i+ii)
  end
  for ii in row_a
    print "\x1b[48;5;#{ii}m  "
  end
  print "\x1b[0m  "
  for ii in row_b
    print "\x1b[48;5;#{ii}m  "
  end
  print "\x1b[0m  "
  for ii in row_c
    print "\x1b[48;5;#{ii}m  "
  end
  puts "\x1b[0m"
end
puts

# Finally, the 24 grays
for i in 232...256
  print "\x1b[48;5;#{i}m  "
end
puts "\x1b[0m"
puts
