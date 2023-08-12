#![allow(dead_code, unused)]

use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;
use std::process::ExitCode;
use std::error;

use nix::libc::{ioctl, TIOCGWINSZ};
use nix::pty::Winsize;
use nix::sys::ioctl::*;
use termios::*;

macro_rules! ctrl_key {
    ($k:expr) => {
        $k & 0x1f
    };
}

#[derive(Debug)]
struct RawMode(Termios);

impl Drop for RawMode {
    fn drop(&mut self) {
        disable_raw_mode(self);
    }
}

#[derive(Debug)]
struct EditorConfig {
    cx: u16,
    cy: u16,
    screenrows: u16,
    screencols: u16,
    orig_termios: RawMode,
}

fn die(s: &str, e: io::Error) {
    _ = io::stdout().write(b"\x1b[2J");
    _ = io::stdout().write(b"\x1b[H");

    panic!("{}: {}", s, e);
}

fn disable_raw_mode(orig_termios: &mut RawMode) {
    tcsetattr(io::stdin().as_raw_fd(), TCSAFLUSH, &orig_termios.0).unwrap();
    // println!("Editor Closed.");
}

fn enable_raw_mode() -> EditorConfig {
    let orig_termios = Termios::from_fd(io::stdin().as_raw_fd()).unwrap();
    let mut raw = orig_termios;

    raw.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= !(OPOST);
    raw.c_cflag |= CS8;
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    tcsetattr(io::stdin().as_raw_fd(), TCSAFLUSH, &raw).unwrap();

    EditorConfig {
        cx : 0,
        cy : 0,
        screenrows : 0,
        screencols : 0,
        orig_termios : RawMode(orig_termios),
    }
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

fn get_cursor_position() -> Option<(u16, u16)> {
    let mut buf = [0u8; 32];
    let mut i = 0;

    if io::stdout().write(b"\x1b[6n").is_err() {
        return None;
    }

    while i < buf.len() - 1 {
        if io::stdin().read(&mut buf).unwrap() != 1 {
            break;
        }
        if buf[i] == b'R' {
            break;
        }
        i += 1;
    }

    if buf[0] != b'\x1b' || buf[1] != b'[' {
        return None;
    }

    let slice = &buf[2..i];
    let pos = std::str::from_utf8(slice).ok()?;
    let parts: Vec<&str> = pos.split(';').collect();

    if parts.len() != 2 {
        return None;
    }
    let x = parts[0].parse::<u16>().ok()?;
    let y = parts[1].parse::<u16>().ok()?;

    println!("{}, {}", x, y);

    Some((x, y))
}

fn get_window_size() -> Option<(u16, u16)> {
    let mut ws = Winsize { ws_row: 0, ws_col: 0, ws_xpixel: 0, ws_ypixel: 0 };

    unsafe {
        if (ioctl(io::stdout().as_raw_fd(), TIOCGWINSZ, &mut ws) == -1 || ws.ws_col == 0) {
            if io::stdout().write(b"\x1b[999C\x1b[999B").is_err() {
                return None;
            }
            return get_cursor_position();
        }
    };

    Some((ws.ws_row, ws.ws_col))
}

fn editor_draw_rows(conf: &EditorConfig) {
    for y in 0..conf.screenrows {
        _ = io::stdout().write(b"~");

        if y < conf.screenrows - 1 {
            _ = io::stdout().write(b"\r\n");
        }
    }
}

fn editor_refresh_screen(conf: &EditorConfig) {
    _ = io::stdout().write(b"\x1b[2J");
    _ = io::stdout().write(b"\x1b[H");

    editor_draw_rows(&conf);

    _ = io::stdout().write(b"\x1b[H");
}

fn editor_process_keypress() -> Option<ExitCode> {
    let c = editor_read_key();

    if c == ctrl_key!(b'q') {
        _ = io::stdout().write(b"\x1b[2J");
        _ = io::stdout().write(b"\x1b[H");
        return Some(ExitCode::SUCCESS);
    }

    None
}

fn init_editor(conf: &mut EditorConfig) -> bool {
    let Some((row, col)) = get_window_size() else {
        return false;
    };

    conf.screenrows = row;
    conf.screencols = col;
    true
}

fn main() {
    let mut conf: EditorConfig = enable_raw_mode();
    if !init_editor(&mut conf) {
        return;
    }

    loop {
        editor_refresh_screen(&conf);
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

