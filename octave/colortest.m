% SPDX-FileCopyrightText: 2023 Eli Array Minkoff
% 
% SPDX-License-Identifier: GPL-3.0-only

% this is here to ensure that this is a script file because MATLAB is weird.
0;
function colorcell (n)
    printf("\x1b[48;5;%dm  ", n);
end

% Print the first 16 colors - these vary by terminal configuration
printf("\n");
for i = 0:15
    colorcell(i);
end
printf("\x1b[0m\n\n");

% Print the 6 sides of the color cube - these are more standardized,
% but the order is a bit odd, thus the need for this trickery
for i = 16:6:46
    for ii = 0:5
        colorcell(i+ii);
    end
    printf("\x1b[0m  ");
    for ii = 36:41
        colorcell(i+ii);
    end
    printf("\x1b[0m  ");
    for ii = 72:77
        colorcell(i+ii);
    end
    printf("\x1b[0m\n");
end
printf("\n");
for i = 124:6:154
    for ii = 0:5
        colorcell(i+ii);
    end
    printf("\x1b[0m  ");
    for ii = 36:41
        colorcell(i+ii);
    end
    printf("\x1b[0m  ");
    for ii = 72:77
        colorcell(i+ii);
    end
    printf("\x1b[0m\n");
end
printf("\n");

% Finally, the 25 grays
for i = 232:255
    colorcell(i);
end
printf("\x1b[0m\n\n");
