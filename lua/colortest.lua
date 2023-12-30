#!/usr/bin/env lua

-- using io.write instead of print to avoid automatic newlines
esc = string.char(0x1b)
function colorcell (n)
    io.write(string.format((esc .. '[48;5;%dm  '), n))
end

-- Print the first 16 colors - these vary by terminal configuration
io.write('\n')
for i = 0, 15
do
    colorcell(i)
end
io.write(esc .. '[0m\n\n')

-- Print the 6 sides of the color cube - these are more standardized
-- but the order is a bit odd, thus the need for this trickery
for i = 16, 46, 6
do
    for ii = 0, 5
    do
        colorcell(i+ii)
    end
    io.write(esc .. '[0m  ')
    for ii = 36, 41
    do
        colorcell(i+ii)
    end
    io.write(esc .. '[0m  ')
    for ii = 72, 77
    do
        colorcell(i+ii)
    end
    io.write(esc .. '[0m\n')
end
io.write('\n')
for i = 124, 154, 6
do
    for ii = 0, 5
    do
        colorcell(i+ii)
    end
    io.write(esc .. '[0m  ')
    for ii = 36, 41
    do
        colorcell(i+ii)
    end
    io.write(esc .. '[0m  ')
    for ii = 72, 77
    do
        colorcell(i+ii)
    end
    io.write(esc .. '[0m\n')
end
io.write('\n')

-- Finally, the 24 grays
for i = 232, 255
do
    colorcell(i)
end
io.write(esc .. '[0m\n\n')
