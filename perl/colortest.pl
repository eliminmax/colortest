#!/usr/bin/env perl

# SPDX-FileCopyrightText: 2022 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

use strict;
use warnings;

# Print the first 16 colors - these vary by terminal configuration
printf "\n" ;
for (my $i = 0; $i < 16; $i++) {
    printf "\e[48;5;%dm  ", $i ;
}
printf "\e[0m\n\n";

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for (my $i = 16; $i < 52; $i+=6) {
    for (my $ii = 0; $ii < 6; $ii++) {
        printf "\e[48;5;%dm  ", ($i+$ii);
    }
    printf "\e[0m  ";
    for (my $ii = 36; $ii < 42; $ii++) {
        printf "\e[48;5;%dm  ", ($i+$ii);
    }
    printf "\e[0m  ";
    for (my $ii = 72; $ii < 78; $ii++) {
        printf "\e[48;5;%dm  ", ($i+$ii);
    }
    printf "\e[0m\n";
}
printf "\n";
for (my $i = 124; $i < 160; $i+=6) {
    for (my $ii = 0; $ii < 6; $ii++) {
        printf "\e[48;5;%dm  ", ($i+$ii);
    }
    printf "\e[0m  ";
    for (my $ii = 36; $ii < 42; $ii++) {
        printf "\e[48;5;%dm  ", ($i+$ii);
    }
    printf "\e[0m  ";
    for (my $ii = 72; $ii < 78; $ii++) {
        printf "\e[48;5;%dm  ", ($i+$ii);
    }
    printf "\e[0m\n";
}
printf "\n";

# Finally, the 24 grays
for (my $i = 232; $i < 256; $i++) {
    printf "\e[48;5;%dm  ", $i;
}
printf "\e[0m\n\n"
