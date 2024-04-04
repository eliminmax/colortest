#!/usr/bin/env r

# SPDX-FileCopyrightText: 2023 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

# Print the first 16 colors - these vary by terminal configuration
cat("\n")
for (i in 0:15) {
    cat("\x1b[48;5;")
    cat(i)
    cat("m  ")
}
cat ("\x1b[0m\n\n")

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for (i in seq(16, 46, 6)) {
    for (ii in 0:5) {
        cat("\x1b[48;5;")
        cat(i+ii)
        cat("m  ")
    }
    cat("\x1b[0m  ")
    for (ii in 36:41) {
        cat("\x1b[48;5;")
        cat(i+ii)
        cat("m  ")
    }
    cat("\x1b[0m  ")
    for (ii in 72:77) {
        cat("\x1b[48;5;")
        cat(i+ii)
        cat("m  ")
    }
    cat("\x1b[0m\n")
}
cat("\n")
for (i in seq(124, 154, 6)) {
    for (ii in 0:5) {
        cat("\x1b[48;5;")
        cat(i+ii)
        cat("m  ")
    }
    cat("\x1b[0m  ")
    for (ii in 36:41) {
        cat("\x1b[48;5;")
        cat(i+ii)
        cat("m  ")
    }
    cat("\x1b[0m  ")
    for (ii in 72:77) {
        cat("\x1b[48;5;")
        cat(i+ii)
        cat("m  ")
    }
    cat("\x1b[0m\n")
}
cat("\n")

# finally, the 24 grays
for (i in 232:255) {
    cat("\x1b[48;5;")
    cat(i)
    cat("m  ")
}
cat("\x1b[0m\n\n")
