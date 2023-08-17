void main () {
    // Print the first 16 colors - these vary by terminal configuration
    stdout.printf ("\n");
    for (int i = 0; i < 16; i++) {
        stdout.printf ("\x1b[48;5;%dm  ", i);
    }
    stdout.printf("\x1b[0m\n\n");

    // Print the 6 sides of the color cube - these are more standardized,
    // but the order is a bit odd, thus the need for this trickery
    for (int i = 16; i < 52; i+=6) {
        for (int ii = 0; ii < 6; ii++) {
            stdout.printf("\x1b[48;5;%dm  ", i+ii);
        }
        stdout.printf("\x1b[0m  ");
        for (int ii = 36; ii < 42; ii++) {
            stdout.printf("\x1b[48;5;%dm  ", i+ii);
        }
        stdout.printf("\x1b[0m  ");
        for (int ii = 72; ii < 78; ii++) {
            stdout.printf("\x1b[48;5;%dm  ", i+ii);
        }
        stdout.printf("\x1b[0m\n");
    }
    stdout.printf("\n");
    for (int i = 124; i < 160; i+=6) {
        for (int ii = 0; ii < 6; ii++) {
            stdout.printf("\x1b[48;5;%dm  ", i+ii);
        }
        stdout.printf("\x1b[0m  ");
        for (int ii = 36; ii < 42; ii++) {
            stdout.printf("\x1b[48;5;%dm  ", i+ii);
        }
        stdout.printf("\x1b[0m  ");
        for (int ii = 72; ii < 78; ii++) {
            stdout.printf("\x1b[48;5;%dm  ", i+ii);
        }
        stdout.printf("\x1b[0m\n");
    }
    stdout.printf("\n");

    // Finally, the 24 grays
    for (int i = 232; i < 256; i++) {
        stdout.printf ("\x1b[48;5;%dm  ", i);
    }
    stdout.printf("\x1b[0m\n\n");
}
