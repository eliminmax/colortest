using System;
namespace colortest {
    class colortest {
        // Print the first 16 colors - these vary by terminal configuration
        public static void Main(string[] args) {
            Console.WriteLine();
            for (int i = 0; i < 16; i++) {
                Console.Write("\x1b[48;5;{0}m  ", i);
            }
            Console.WriteLine("\x1b[0m\n"); // Double new-line is intentional
            // Print the 6 sides of the color cube - these are more standardized
            // but the order is a bit odd, thus the need for this trickery
            for (int i = 16; i < 52; i+=6) {
                for(int ii = 0; ii < 6; ii++) {
                    Console.Write("\x1b[48;5;{0}m  ", i+ii);
                }
                Console.Write("\x1b[0m  ");
                for(int ii = 36; ii < 42; ii++) {
                    Console.Write("\x1b[48;5;{0}m  ", i+ii);
                }
                Console.Write("\x1b[0m  ");
                for(int ii = 72; ii < 78; ii++) {
                    Console.Write("\x1b[48;5;{0}m  ", i+ii);
                }
                Console.WriteLine("\x1b[0m");
            }
            Console.WriteLine();
            for (int i = 124; i < 160; i+=6) {
                for(int ii = 0; ii < 6; ii++) {
                    Console.Write("\x1b[48;5;{0}m  ", i+ii);
                }
                Console.Write("\x1b[0m  ");
                for(int ii = 36; ii < 42; ii++) {
                    Console.Write("\x1b[48;5;{0}m  ", i+ii);
                }
                Console.Write("\x1b[0m  ");
                for(int ii = 72; ii < 78; ii++) {
                    Console.Write("\x1b[48;5;{0}m  ", i+ii);
                }
                Console.WriteLine("\x1b[0m");
            }
            Console.WriteLine();
            // Finally, the 24 grays
            for(int i = 232; i < 256; i++) {
                Console.Write("\x1b[48;5;{0}m  ", i);
            }
            Console.WriteLine("\x1b[0m\n");
        }
    }
}
