#!/usr/bin/env -S awk -f
BEGIN {

    # Print the first 16 colors - these vary by terminal configuration
    print
    for (i = 0; i < 16; i++)
        printf "\x1b[48;5;%dm  ", i
    printf "\x1b[0m\n"
    print

    # Print the 6 sides of the color cube - these are more standardized
    # but the order is a bit odd, thus the need for this trickery
    for (i = 16; i < 52; i += 6) {
        for (ii = 0; ii < 6; ii++)
            printf "\x1b[48;5;%dm  ", i + ii
        printf "\x1b[0m  "
        for (ii = 36; ii < 42; ii++)
            printf "\x1b[48;5;%dm  ", i + ii
        printf "\x1b[0m  "
        for (ii = 72; ii < 78; ii++)
            printf "\x1b[48;5;%dm  ", i + ii
        print "\x1b[0m"
    }
    print
    for (i = 124; i < 160; i += 6) {
        for (ii = 0; ii < 6; ii++)
            printf "\x1b[48;5;%dm  ", i + ii
        printf "\x1b[0m  "
        for (ii = 36; ii < 42; ii++)
            printf "\x1b[48;5;%dm  ", i + ii
        printf "\x1b[0m  "
        for (ii = 72; ii < 78; ii++)
            printf "\x1b[48;5;%dm  ", i + ii
        print "\x1b[0m"
    }
    print

    # Finally, the 24 grays
    for(i = 232; i < 256; i++)
        printf "\x1b[48;5;%dm  ", i
    print "\x1b[0m\n"
}
