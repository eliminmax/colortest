// SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

object colortest {
  def colorCell(n: Int) = f"\u001b[48;5;$n%dm  "

  def cubeRowPart(n: Int) = {
    ((n to (n+5)).map(colorCell).mkString("")) + "\u001b[0m"
  }
  def cubeRow(n: Int) = {
    (0 to 2).map(i => n + (i*36)).map(cubeRowPart).mkString("  ") + "\n"
  }
  def main(args: Array[String]) = {
    // Print the first 16 colors - these vary by terminal configuration
    print("\n" + (0 to 15).map(colorCell).mkString("") + "\u001b[0m\n\n")

    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for the above trickery
    print((16 to 46 by 6).map(cubeRow).mkString("") + "\n")
    print((124 to 154 by 6).map(cubeRow).mkString("") + "\n")

    // Finally, the 24 grays
    print((232 to 255).map(colorCell).mkString("") + "\u001b[0m\n\n")
  }
}
