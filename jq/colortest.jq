#!/usr/bin/env -S jq -njf
# SPDX-FileCopyrightText: 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# if run with -n, it expects no input
# if run with -j, it outputs everything as raw data (no trailing "\n")
# if run with -f, it reads code from a file rather than the command line

# jq outputs whatever's left at the end, and defaults to doing it as JSON
# because it's called with -j, however, it outputs the string that's
# constructed, but only once it's done constructing it.

def colorCell: "\u001b[48;5;" + (.|tostring) + "m  ";

def cubeRowPart: ([range(.; .+6)] | map(colorCell) | join("")) + "\u001b[0m";
def cubeRow: ([., .+36, .+72] | map(cubeRowPart) | join("  "));


# the first 16 colors - these vary by terminal configuration
"\n" + ([range(16)] | map(colorCell) | join("")) + "\u001b[0m\n\n" +

# the 6 sides of the color cube - these are more standardized,
# but the order is a bit odd, thus the need for this trickery
([range(16; 52; 6)] | map(cubeRow) | join("\n")) + "\n\n" +
([range(124; 160; 6)] | map(cubeRow) | join("\n")) + "\n\n" +

# finally, the 24 grays
([range(232;256)] | map(colorCell) | join("")) + "\u001b[0m\n\n"
