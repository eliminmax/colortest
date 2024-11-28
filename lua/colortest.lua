#!/usr/bin/env lua
-- SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
--
-- SPDX-License-Identifier: GPL-3.0-only

-- using io.write throughout instead of print to avoid automatic newlines

esc = string.char(0x1b)

function color_cell (n)
    io.write(string.format((esc .. '[48;5;%dm  '), n))
end

function cube_row_part (n)
    for i = n, n+5
    do
        color_cell(i)
    end
end

function cube_row (n)
    cube_row_part(n)
    io.write(esc .. '[0m  ')
    cube_row_part(n + 36)
    io.write(esc .. '[0m  ')
    cube_row_part(n + 72)
    io.write(esc .. '[0m\n')
end

-- Print the first 16 colors - these vary by terminal configuration
io.write('\n')
for i = 0, 15
do
    color_cell(i)
end
io.write(esc .. '[0m\n\n')

-- Print the 6 sides of the color cube - these are more standardized
-- but the order is a bit odd, thus the need for this trickery
for i = 16, 46, 6
do
    cube_row(i)
end
io.write('\n')
for i = 124, 154, 6
do
    cube_row(i)
end
io.write('\n')

-- Finally, the 24 grays
for i = 232, 255
do
    color_cell(i)
end
io.write(esc .. '[0m\n\n')
