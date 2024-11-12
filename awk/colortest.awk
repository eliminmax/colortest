#!/usr/bin/env -S awk -f

# SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# parameters can't mirror variable names, and variables are all global
# workaround: ugly initialisms of function names as variable/parameter prefixes
function color_cell(ccn) { printf "\033[48;5;%dm  ", ccn }
function cube_row_part(crpn) {
    for(crpi = crpn; crpi < (crpn + 6); crpi++) color_cell(crpi)
    printf("\033[0m")
}
function cube_row(crn) {
    cube_row_part(crn)
    printf("  ")
    cube_row_part(crn + 36)
    printf("  ")
    cube_row_part(crn + 72)
    print
}

BEGIN {
    # Print the first 16 colors - these vary by terminal configuration
    print
    for (i = 0; i < 16; i++) color_cell(i)
    print "\033[0m\n" # extra newline appended, which is intended

    # Print the 6 sides of the color cube - these are more standardized
    # but the order is a bit odd, thus the need for the above trickery
    for (i = 16; i < 52; i += 6) cube_row(i)
    print
    for (i = 124; i < 160; i += 6) cube_row(i)
    print

    # Finally, the 24 grays
    for(i = 232; i < 256; i++) color_cell(i)
    print "\033[0m\n" # extra newline appended, which is intended
}
