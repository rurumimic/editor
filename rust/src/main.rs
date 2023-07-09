use std::io::{self, Read};
use std::os::unix::io::AsRawFd;

use termios::*;

macro_rules! ctrl_key {
    ($k:expr) => {
        $k & 0x1f
    };
}

struct RawMode(Termios);

impl Drop for RawMode {
    fn drop(&mut self) {
        disable_raw_mode(self);
    }
}

fn disable_raw_mode(orig_termios: &mut RawMode) {
    tcsetattr(io::stdin().as_raw_fd(), TCSAFLUSH, &orig_termios.0).unwrap();
}

fn enable_raw_mode() -> RawMode {
    let orig_termios = Termios::from_fd(io::stdin().as_raw_fd()).unwrap();
    let mut raw = orig_termios;

    raw.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= !(OPOST);
    raw.c_cflag |= CS8;
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    tcsetattr(io::stdin().as_raw_fd(), TCSAFLUSH, &raw).unwrap();

    return RawMode(orig_termios);
}

fn main() {
    let _raw_mode = enable_raw_mode();

    loop {
        let mut c = [0u8; 1];

        match io::stdin().read_exact(&mut c) {
            Ok(_) => {}
            Err(e) => {
                if e.kind() != io::ErrorKind::UnexpectedEof {
                    panic!("read error: {}", e);
                }
            }
        }

        if c[0].is_ascii_control() {
            print!("{}\r\n", c[0]);
        } else {
            print!("{:?} ('{}')\r\n", c, c[0] as char);
        }

        if c[0] == ctrl_key!(b'q') {
            break;
        }
    }
}
