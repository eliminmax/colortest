object colortest {
  def main(args: Array[String]) = {
    // Print the first 16 colors - these vary by terminal configuration
    println()
    for (i <- 0 until 16) {
      print(f"\u001b[48;5;$i%dm  ")
    }
    println("\u001b[0m\n")

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for this trickery
    for (i <- 16 until 52 by 6) {
      for (ii <- 0 until 6) {
        print(f"\u001b[48;5;${i+ii}%dm  ")
      }
      print("\u001b[0m  ")
      for (ii <- 36 until 42) {
        print(f"\u001b[48;5;${i+ii}%dm  ")
      }
      print("\u001b[0m  ")
      for (ii <- 72 until 78) {
        print(f"\u001b[48;5;${i+ii}%dm  ")
      }
      println("\u001b[0m")
    }
    println()
    for (i <- 124 until 160 by 6) {
      for (ii <- 0 until 6) {
        print(f"\u001b[48;5;${i+ii}%dm  ")
      }
      print("\u001b[0m  ")
      for (ii <- 36 until 42) {
        print(f"\u001b[48;5;${i+ii}%dm  ")
      }
      print("\u001b[0m  ")
      for (ii <- 72 until 78) {
        print(f"\u001b[48;5;${i+ii}%dm  ")
      }
      println("\u001b[0m")
    }
    println()

    // Finally, the 24 grays
    for (i <- 232 until 256) {
      print(f"\u001b[48;5;$i%dm  ")
    }
    println("\u001b[0m\n")
  }
}
