% SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
% 
% SPDX-License-Identifier: GPL-3.0-only

% this is here to ensure that this is a script file because MATLAB is weird.
0;
function color_cell (n)
    printf("\x1b[48;5;%dm  ", n);
end

function cube_row_part (n)
    for i = n:(n+5)
        color_cell(i);
    end
end

function cube_row (n)
    cube_row_part(n)
    printf("\x1b[0m  ")
    cube_row_part(n + 36)
    printf("\x1b[0m  ")
    cube_row_part(n + 72)
    printf("\x1b[0m\n")
end

% Print the first 16 colors - these vary by terminal configuration
printf("\n");
for i = 0:15
    color_cell(i);
end
printf("\x1b[0m\n\n");

% Print the 6 sides of the color cube - these are more standardized,
% but the order is a bit odd, thus the need for the above trickery
for i = 16:6:46
    cube_row(i)
end
printf("\n");
for i = 124:6:154
    cube_row(i)
end
printf("\n");

% Finally, the 25 grays
for i = 232:255
    color_cell(i);
end
printf("\x1b[0m\n\n");
