#!/usr/bin/env r

# SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only


color_cell <- function(n) {
    cat("\x1b[48;5;")
    cat(n)
    cat("m  ")
}

cube_row_part <- function(n) for (i in 0:5) color_cell(n + i)
cube_row <- function(n) {
    cube_row_part(n)
    cat("\x1b[0m  ")
    cube_row_part(n + 36)
    cat("\x1b[0m  ")
    cube_row_part(n + 72)
    cat("\x1b[0m\n")
}

# Print the first 16 colors - these vary by terminal configuration
cat("\n")
for (i in 0:15) color_cell(i)
cat ("\x1b[0m\n\n")

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for (i in seq(16, 46, 6)) cube_row(i)
cat("\n")
for (i in seq(124, 154, 6)) cube_row(i)
cat("\n")

# finally, the 24 grays
for (i in 232:255) color_cell(i)
cat("\x1b[0m\n\n")
