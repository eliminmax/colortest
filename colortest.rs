fn main() {
    println!();

    // Print the first 16 colors - these vary by terminal configuration
    for i in 0..16 {
        print!("\x1b[48;5;{}m  ", format!("{}", i));
    }
    println!("\x1b[0m\n");
    // Print the 6 sides of the color cube - these are more standardized
    // but the order is a bit odd, thus the need for this trickery
    for i in (16..52).step_by(6) {
        for ii in 0..6 {
            print!("\x1b[48;5;{}m  ", format!("{}", i + ii));
        }
        print!("\x1b[0m  ");
        for ii in 36..42 {
            print!("\x1b[48;5;{}m  ", format!("{}", i + ii));
        }
        print!("\x1b[0m  ");
        for ii in 72..78 {
            print!("\x1b[48;5;{}m  ", format!("{}", i + ii));
        }
        println!("\x1b[0m");
    }
    println!();
    for i in (124..160).step_by(6) {
        for ii in 0..6 {
            print!("\x1b[48;5;{}m  ", format!("{}", i + ii));
        }
        print!("\x1b[0m  ");
        for ii in 36..42 {
            print!("\x1b[48;5;{}m  ", format!("{}", i + ii));
        }
        print!("\x1b[0m  ");
        for ii in 72..78 {
            print!("\x1b[48;5;{}m  ", format!("{}", i + ii));
        }
        println!("\x1b[0m");
    }
    println!();

    // Finally, the 24 grays
    for i in 232..256 {
        print!("\x1b[48;5;{}m  ", format!("{}", i));
    }
    println!("\x1b[0m\n")
}
