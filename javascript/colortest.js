#!/usr/bin/env node

// SPDX-FileCopyrightText: 2022-2023 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

const util = require('util');

// Print the first 16 colors - these vary by terminal configuration
process.stdout.write('\n')
for(var i = 0; i < 16; i++) {
    // process.stdout.write(util.format()) is basically console.log without 
    // the automatic trailing newline
    process.stdout.write(util.format('\u001b[48;5;%dm  ', i)) 
}
process.stdout.write('\u001b[0m\n\n')

// Print the 6 sides of the color cube - these are more standardized
// but the order is a bit odd, thus the need for this trickery
for(var i = 16; i < 52; i+=6) {
    for(var ii = 0; ii < 6; ii++) {
        process.stdout.write(util.format('\u001b[48;5;%dm  ', i+ii))
    }
    process.stdout.write('\u001b[0m  ')
    for(var ii = 36; ii < 42; ii++) {
        process.stdout.write(util.format('\u001b[48;5;%dm  ', i+ii))
    }
    process.stdout.write('\u001b[0m  ')
    for(var ii = 72; ii < 78; ii++) {
        process.stdout.write(util.format('\u001b[48;5;%dm  ', i+ii))
    }
    process.stdout.write('\u001b[0m\n')
}
process.stdout.write('\n')

for(var i = 124; i < 160; i+=6) {
    for(var ii = 0; ii < 6; ii++) {
        process.stdout.write(util.format('\u001b[48;5;%dm  ', i+ii))
    }
    process.stdout.write('\u001b[0m  ')
    for(var ii = 36; ii < 42; ii++) {
        process.stdout.write(util.format('\u001b[48;5;%dm  ', i+ii))
    }
    process.stdout.write('\u001b[0m  ')
    for(var ii = 72; ii < 78; ii++) {
        process.stdout.write(util.format('\u001b[48;5;%dm  ', i+ii))
    }
    process.stdout.write('\u001b[0m\n')
}
process.stdout.write('\n')

// Finally, the 24 grays
for(var i = 232; i < 256; i++) {
    process.stdout.write(util.format('\u001b[48;5;%dm  ', i))
}
process.stdout.write('\u001b[0m\n\n')
