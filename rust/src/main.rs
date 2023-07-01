use std::io::{self, Read};

fn main() {
    let mut c = [0u8; 1];

    while io::stdin().read_exact(&mut c).is_ok() && c[0] != b'q' {}
}
