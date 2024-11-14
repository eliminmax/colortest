#!/usr/bin/env ts-node

// SPDX-FileCopyrightText: 2022-2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

const util = require('util')

function color_cell(n: number) {
    // process.stdout.write(util.format()) is basically console.log without 
    // the automatic trailing newline
    process.stdout.write(util.format("\u001b[48;5;%dm  ", n))
}

function cube_row_part(n: number) {
    for (var i = n; i < n + 6; i++) color_cell(i)
}

function cube_row(n: number) {
    cube_row_part(n)
    process.stdout.write("\u001b[0m  ")
    cube_row_part(n + 36)
    process.stdout.write("\u001b[0m  ")
    cube_row_part(n + 72)
    process.stdout.write("\u001b[0m\n")
}

// Print the first 16 colors - these vary by terminal configuration
process.stdout.write('\n')
for (let i: number = 0; i < 16; i++) color_cell(i)
process.stdout.write('\u001b[0m\n\n')

// Print the 6 sides of the color cube - these are more standardized
// but the order is a bit odd, thus the need for the above trickery
for (let i: number = 16; i < 52; i+=6) cube_row(i)
process.stdout.write('\n')
for(let i: number = 124; i < 160; i+=6) cube_row(i)
process.stdout.write('\n')

// Finally, the 24 grays
for (let i: number = 232; i < 256; i++) color_cell(i)
process.stdout.write('\u001b[0m\n\n')
