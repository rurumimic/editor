#![allow(dead_code, unused)]

use std::error;
use std::cmp;
use std::str;
use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;

use nix::libc::{self, ioctl, TIOCGWINSZ};
use nix::pty::Winsize;
use nix::sys::ioctl::*;
use termios::*;

const EDITOR_VERSION: &'static str = "0.0.1";

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

fn unsafe_write(buf: &str) -> Result<isize, io::Error> {
    let ret = unsafe {
        libc::write(libc::STDOUT_FILENO, buf.as_ptr() as *const libc::c_void, buf.len())
    };

    if ret == -1 {
        Err(io::Error::last_os_error())
    } else {
        Ok(ret)
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
        cx: 0,
        cy: 0,
        screenrows: 0,
        screencols: 0,
        orig_termios: RawMode(orig_termios),
    }
}

fn editor_read_key() -> u8 {
    let mut c = [0u8; 1];

    loop {
        match io::stdin().read_exact(&mut c) {
            Ok(_) => {
                return c[0];
            }
            Err(e) => {
                if e.kind() != io::ErrorKind::UnexpectedEof {
                    die("Read Key Error", e);
                }
            }
        }
    }
}

fn get_cursor_position() -> Option<(u16, u16)> {
    if io::stdout().write(b"\x1b[6n").is_err() {
        return None;
    }
    io::stdout().flush().ok()?;

    let mut buf = Vec::new();
    io::stdin().read_to_end(&mut buf).ok()?;

    if let Some(pos) = String::from_utf8_lossy(&buf).find('R') {
        let row_col = String::from_utf8_lossy(&buf[2..pos]);
        let coordinates: Vec<&str> = row_col.split(';').collect();
        let row = coordinates[0].parse::<usize>().ok()? as u16;
        let col = coordinates[1].parse::<usize>().ok()? as u16;
        return Some((row, col))
    }

    None
}

fn get_window_size() -> Option<(u16, u16)> {
    let mut ws = Winsize {
        ws_row: 0,
        ws_col: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

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

struct Abuf {
    b: String,
}

impl Abuf {
    fn new() -> Abuf {
        Abuf{ b: String::new() }
    }

    fn append(&mut self, s: &str) {
        self.b.push_str(s);
    }
}

fn editor_draw_rows(conf: &EditorConfig, ab: &mut Abuf) {
    for y in 0..conf.screenrows {
        if y == conf.screenrows / 3 {
            let welcome = format!("The Editor -- version {}", EDITOR_VERSION);
            let welcomelen = cmp::min(welcome.len(), conf.screencols.into());
            let mut padding = (conf.screencols - welcomelen as u16) / 2;
            if padding > 0 {
                ab.append("~");
                padding -= 1;
            }
            while padding > 0 {
                ab.append(" ");
                padding -= 1;
            }
            ab.append(&welcome[..welcomelen]);
        } else {
            ab.append("~");
        }

        ab.append("\x1b[K");
        if y < conf.screenrows - 1 {
            ab.append("\r\n");
        }
    }
}

fn editor_refresh_screen(conf: &EditorConfig) {
    let mut ab = Abuf::new();

    ab.append("\x1b[?25l");
    ab.append("\x1b[H");

    editor_draw_rows(&conf, &mut ab);

    let cursor = format!("\x1b[{};{}H", conf.cy + 1, conf.cx + 1);
    ab.append(cursor.as_str());

    ab.append("\x1b[?25h");

    _ = io::stdout().write(ab.b.as_bytes());
    _ = io::stdout().flush();
}

fn editor_move_cursor(key: u8, conf: &mut EditorConfig) {
    match key {
        b'w' => {
            if conf.cy != 0 {
                conf.cy -= 1;
            }
        },
        b's' => {
            if conf.cy != conf.screenrows - 1 {
                conf.cy += 1;
            }
        },
        b'a' => {
            if conf.cx != 0 {
                conf.cx -= 1;
            }
        },
        b'd' => {
            if conf.cx != conf.screencols - 1 {
                conf.cx += 1;
            }
        },
        _ => {},
    }
}

fn editor_process_keypress(conf: &mut EditorConfig) -> bool {
    let c = editor_read_key();

    if c == ctrl_key!(b'q') {
        _ = io::stdout().write(b"\x1b[2J");
        _ = io::stdout().write(b"\x1b[H");
        return false;
    }

    match c {
        b'w'|b's'|b'a'|b'd' => editor_move_cursor(c, conf),
        _ => {},
    }

    true
}

fn init_editor(conf: &mut EditorConfig) -> bool {
    let Some((row, col)) = get_window_size() else {
        return false;
    };

    conf.cx = 0;
    conf.cy = 0;
    conf.screenrows = row;
    conf.screencols = col;
    true
}

fn main() {
    let mut conf: EditorConfig = enable_raw_mode();
    if init_editor(&mut conf) == false {
        return;
    }

    loop {
        editor_refresh_screen(&conf);
        if editor_process_keypress(&mut conf) == false {
            break;
        }
    }
}
