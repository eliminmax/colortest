#!/usr/bin/env node
const util = require('util');
// Print the first 16 colors - these vary by terminal configuration
console.log()
for(var i = 0; i < 16; i++) {
    //process.stdout.write(util.format()) is console.log without the newline
    process.stdout.write(util.format('\u001b[48;5;%dm  ', i)) 
}
console.log('\u001b[0m')
console.log()

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
    console.log('\u001b[0m')
}
console.log()

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
    console.log('\u001b[0m')
}
console.log()

// Finally, the 24 grays
for(var i = 232; i < 256; i++) {
    process.stdout.write(util.format('\u001b[48;5;%dm  ', i))
}
console.log('\u001b[0m\n')
