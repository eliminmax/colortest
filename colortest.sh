#!/bin/sh
# Print the first 16 colors - these vary by terminal configuration
printf '\n'
i=0
while [ $i -lt 16 ]; do
    printf "\033[48;5;%dm  " $i
    i=$((i + 1))
done
printf "\033[0m\n\n"
# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
i=16
while [ $i -lt 52 ]; do
    ii=0
    while [ $ii -lt 6 ]; do
        printf "\033[48;5;%dm  " $((i + ii))
        ii=$((ii + 1))
    done
    printf "\033[0m  "
    ii=0
    while [ $ii -lt 6 ]; do
        printf "\033[48;5;%dm  " $((i + ii + 36))
        ii=$((ii + 1))
    done
    printf "\033[0m  "
    ii=0
    while [ $ii -lt 6 ]; do
        printf "\033[48;5;%dm  " $((i + ii + 72))
        ii=$((ii + 1))
    done
    printf "\033[0m\n"
    i=$((i + 6))
done
printf '\n'
i=124
while [ $i -lt 160 ]; do
    ii=0
    while [ $ii -lt 6 ]; do
        printf "\033[48;5;%dm  " $((i + ii))
        ii=$((ii + 1))
    done
    printf "\033[0m  "
    ii=0
    while [ $ii -lt 6 ]; do
        printf "\033[48;5;%dm  " $((i + ii + 36))
        ii=$((ii + 1))
    done
    printf "\033[0m  "
    ii=0
    while [ $ii -lt 6 ]; do
        printf "\033[48;5;%dm  " $((i + ii + 72))
        ii=$((ii + 1))
    done
    printf "\033[0m\n"
    i=$((i + 6))
done
printf '\n'

# Finally, the 24 grays
i=232
while [ $i -lt 256 ]; do
    printf "\033[48;5;%dm  " $i
    i=$((i + 1))
done
printf '\033[0m\n\n'
