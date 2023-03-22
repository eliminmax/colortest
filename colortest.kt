fun main() {
    // Print the first 16 colors - these vary by terminal configuration
    println()
    for (i in 0..15) {
        print("\u001b[48;5;${i}m  ")
    }
    println("\u001b[0m\n")

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for this trickery
    for (i in 16..46 step 6) {
        for (ii in 0..5) {
            print("\u001b[48;5;${i+ii}m  ")
        }
        print("\u001b[0m  ")
        for (ii in 36..41) {
            print("\u001b[48;5;${i+ii}m  ")
        }
        print("\u001b[0m  ")
        for (ii in 72..77) {
            print("\u001b[48;5;${i+ii}m  ")
        }
        println("\u001b[0m")
    }
    println()
    for (i in 124..154 step 6) {
        for (ii in 0..5) {
            print("\u001b[48;5;${i+ii}m  ")
        }
        print("\u001b[0m  ")
        for (ii in 36..41) {
            print("\u001b[48;5;${i+ii}m  ")
        }
        print("\u001b[0m  ")
        for (ii in 72..77) {
            print("\u001b[48;5;${i+ii}m  ")
        }
        println("\u001b[0m")
    }
    println()

    // Finally, the 24 grays
    for (i in 232..255) {
        print("\u001b[48;5;${i}m  ")
    }
    println("\u001b[0m\n")
}
