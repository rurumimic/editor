#![allow(dead_code, unused)]

use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;
use std::process::ExitCode;
use std::error;

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

fn die(s: &str, e: io::Error) {
    _ = io::stdout().write(b"\x1b[2J");
    _ = io::stdout().write(b"\x1b[H");

    panic!("{}: {}", s, e);
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

fn editor_read_key() -> u8 {
    let mut c = [0u8; 1];

    loop {
        match io::stdin().read_exact(&mut c) {
            Ok(_) => { return c[0]; },
            Err(e) => {
                if e.kind() != io::ErrorKind::UnexpectedEof {
                    die("Read Key Error", e);
                }
            }
        }
    }
}

fn editor_refresh_screen() {
    _ = io::stdout().write(b"\x1b[2J");
    _ = io::stdout().write(b"\x1b[H");
}

fn editor_process_keypress() -> Option<ExitCode> {
    let c = editor_read_key();

    if c == ctrl_key!(b'q') {
        _ = io::stdout().write(b"\x1b[2J");
        _ = io::stdout().write(b"\x1b[H");
        return Some(ExitCode::SUCCESS);
    }

    return None;
}

fn main() {
    let _raw_mode = enable_raw_mode();

    loop {
        editor_refresh_screen();
        if editor_process_keypress().is_some() {
            break;
        }

        /*
        if c[0].is_ascii_control() {
            print!("{}\r\n", c[0]);
        } else {
            print!("{:?} ('{}')\r\n", c, c[0] as char);
        }
        */
    }

}
