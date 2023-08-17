public class colortest {
    public static void main(String[] args) {
        // Print the first 16 colors - these vary by terminal configuration
        System.out.println();
        for(int i = 0; i < 16; i++) {
            System.out.print("\033[48;5;" + Integer.toString(i) + "m  ");
        }
        System.out.println("\033[0m\n"); // Double new-line is intentional

        // Print the 6 sides of the color cube - these are more standardized
        // but the order is a bit odd, thus the need for this trickery
        for(int i = 16; i < 52; i+=6) {
            for(int ii = 0; ii < 6; ii++) {
                System.out.print("\033[48;5;" + Integer.toString(i+ii) + "m  ");
            }
            System.out.print("\033[0m  ");
            for(int ii = 36; ii < 42; ii++) {
                System.out.print("\033[48;5;" + Integer.toString(i+ii) + "m  ");
            }
            System.out.print("\033[0m  ");
            for(int ii = 72; ii < 78; ii++) {
                System.out.print("\033[48;5;" + Integer.toString(i+ii) + "m  ");
            }
            System.out.println("\033[0m");
        }
        System.out.println();
        for(int i = 124; i < 160; i+=6) {
            for(int ii = 0; ii < 6; ii++) {
                System.out.print("\033[48;5;" + Integer.toString(i+ii) + "m  ");
            }
            System.out.print("\033[0m  ");
            for(int ii = 36; ii < 42; ii++) {
                System.out.print("\033[48;5;" + Integer.toString(i+ii) + "m  ");
            }
            System.out.print("\033[0m  ");
            for(int ii = 72; ii < 78; ii++) {
                System.out.print("\033[48;5;" + Integer.toString(i+ii) + "m  ");
            }
            System.out.println("\033[0m");
        }
        System.out.println();

        // Finally, the 24 grays
        for(int i = 232; i < 256; i++) {
            System.out.print("\033[48;5;"+Integer.toString(i) + "m  ");
        }
        System.out.println("\033[0m\n");
    }
}
