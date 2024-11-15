#!/usr/bin/env -S jq -njf
# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# jq outputs whatever's left at the end, and defaults to doing it as JSON
# because it's called with -j, however, it outputs the string that's
# constructed, but only once it's done constructing it.

def colorCells:
    ([.[] | "\u001b[48;5;" + (. | tostring) + "m  " ] | add) + "\u001b[0m"
;

def cubeRow: [., .+36, .+72] | map([range(.; .+6)] | colorCells) | join("  ");


# the first 16 colors - these vary by terminal configuration
"\n" + ([range(16)] | colorCells) + "\n\n" +

# the 6 sides of the color cube - these are more standardized,
# but the order is a bit odd, thus the need for this trickery
([range(16; 52; 6)] | map(cubeRow) | join("\n")) + "\n\n" +
([range(124; 160; 6)] | map(cubeRow) | join("\n")) + "\n\n" +

# finally, the 24 grays
([range(232;256)] | colorCells) + "\n\n"
