#include <stdio.h>

#define ESC "\x1b"


int main()
{
    // Print the first 16 colors - these vary by terminal configuration
    printf("\n");
    for(int i = 0; i < 16; i++)
    {
        printf(ESC "[48;5;%dm  ", i);
    }
    printf(ESC "[0m\n\n");

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for this trickery
    for(int i = 16; i < 52; i += 6)
    {
        for(int ii = 0; ii < 6; ii++)
        {
            printf(ESC "[48;5;%dm  ", i + ii);
        }
        printf(ESC "[0m  ");
        for(int ii = 0; ii < 6; ii++)
        {
            printf(ESC "[48;5;%dm  ", i + ii+ 36);
        }
        printf(ESC "[0m  ");
        for(int ii = 0; ii < 6; ii++)
        {
            printf(ESC "[48;5;%dm  ", i + ii + 72);
        }
        printf(ESC "[0m\n");

    }
    printf("\n");
    for(int i = 124; i < 160; i += 6)
    {
        for(int ii = 0; ii < 6; ii++)
        {
            printf(ESC "[48;5;%dm  ", i + ii);
        }
        printf(ESC "[0m  ");
        for(int ii = 0; ii < 6; ii++)
        {
            printf(ESC "[48;5;%dm  ", i + ii+ 36);
        }
        printf(ESC "[0m  ");
        for(int ii = 0; ii < 6; ii++)
        {
            printf(ESC "[48;5;%dm  ", i + ii + 72);
        }
        printf(ESC "[0m\n");

    }
    printf("\n");
    // Finally, the 24 grays
    for(int i = 232; i < 256; i++)
    {
        printf(ESC "[48;5;%dm  ", i);
    }

    printf(ESC "[0m\n\n");

    return 0;
}
