#!/usr/bin/env php
<?php

/* SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
   SPDX-License-Identifier: GPL-3.0-only */

function color_cell($n) {
    echo "\x1b[48;5;${n}m  ";
}

function cube_row_part($n) {
    for($i = $n; $i < $n + 6; $i++) color_cell($i);
}

function cube_row($n) {
    cube_row_part($n);
    echo "\x1b[0m  ";
    cube_row_part($n + 36);
    echo "\x1b[0m  ";
    cube_row_part($n + 72);
    echo "\x1b[0m\n";
}


/* Print the first 16 colors - these vary by terminal configuration */
echo "\n";
for ($i = 0; $i < 16; $i++) color_cell($i);
echo "\x1b[0m\n\n";

/*  Print the 6 sides of the color cube - these are more standardized
    but the order is a bit odd, thus the need for this trickery */
for($i = 16; $i < 52; $i+=6) cube_row($i);
echo "\n";
for($i = 124; $i < 160; $i+=6) cube_row($i);
echo "\n";

/* Finally, the 24 grays */
for($i = 232; $i < 256; $i++) color_cell($i);
echo "\x1b[0m\n\n"

?>
