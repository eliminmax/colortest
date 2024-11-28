#!/bin/sh

# SPDX-FileCopyrightText: 2022 - 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

color_cell () {
    printf '\033[48;5;%dm  ' "$1"
}

cube_row_part () {
    crpi="$1"; while [ "$crpi" -lt $(( $1 + 6 )) ]; do
        color_cell "$crpi"
        crpi=$(( crpi + 1 ))
    done
}

cube_row () {
    cube_row_part "$1"
    printf '\033[0m  '
    cube_row_part $(( $1 + 36 ))
    printf '\033[0m  '
    cube_row_part $(( $1 + 72 ))
    printf '\033[0m\n'
}

# Print the first 16 colors - these vary by terminal configuration
printf '\n'
i=0; while [ "$i" -lt 16 ]; do color_cell "$i"; i=$(( i + 1 )); done
printf "\033[0m\n\n"

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
i=16; while [ "$i" -lt 52 ]; do
    cube_row "$i"
    i=$(( i + 6 ))
done
printf '\n'
i=124; while [ "$i" -lt 160 ]; do
    cube_row "$i"
    i=$(( i + 6 ))
done
printf '\n'

# Finally, the 24 grays
i=232
while [ "$i" -lt 256 ]; do
    color_cell "$i"
    i=$(( i + 1 ))
done
printf '\033[0m\n\n'
