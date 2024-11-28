#!/usr/bin/env perl

# SPDX-FileCopyrightText: 2022 - 2024 Eli Array Minkoff
#
# SPDX-License-Identifier: GPL-3.0-only

use strict;
use warnings;

sub color_cell {
    printf "\e[48;5;%dm  ", $_[0];
}

sub cube_row_part {
    for (my $i = $_[0]; $i < $_[0] + 6; $i++) {
        color_cell $i;
    }
}
sub cube_row {
    cube_row_part $_[0];
    print "\x1b[0m  ";
    cube_row_part ($_[0] + 36);
    print "\x1b[0m  ";
    cube_row_part ($_[0] + 72);
    print "\x1b[0m\n";
}

# Print the first 16 colors - these vary by terminal configuration
print "\n" ;
for (my $i = 0; $i < 16; $i++) {
    color_cell $i
}
print "\e[0m\n\n";

# Print the 6 sides of the color cube - these are more standardized
# but the order is a bit odd, thus the need for this trickery
for (my $i = 16; $i < 52; $i+=6) {
    cube_row $i
}
print "\n";
for (my $i = 124; $i < 160; $i+=6) {
    cube_row $i
}
print "\n";

# Finally, the 24 grays
for (my $i = 232; $i < 256; $i++) {
    color_cell $i
}
print "\e[0m\n\n"
