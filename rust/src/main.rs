#![allow(dead_code, unused)]

use std::cmp;
use std::env;
use std::error;
use std::fs::File;
use std::io::{self, BufRead, Read, Write};
use std::os::unix::io::AsRawFd;
use std::str;
use std::time::{Duration, SystemTime};

use nix::libc::{self, ioctl, TIOCGWINSZ};
use nix::pty::Winsize;
use nix::sys::ioctl::*;
use termios::*;

const EDITOR_VERSION: &'static str = "0.0.1";
const EDITOR_TAB_STOP: u16 = 8;

macro_rules! ctrl_key {
    ($k:expr) => {
        $k & 0x1f
    };
}

#[repr(u8)]
enum EditorKey {
    Key(u8),
    Esc = b'\x1b',
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    DelKey,
    HomeKey,
    EndKey,
    PageUp,
    PageDown,
}

#[derive(Debug)]
struct RawMode(Termios);

impl Drop for RawMode {
    fn drop(&mut self) {
        disable_raw_mode(self);
    }
}

#[derive(Debug)]
struct Erow {
    size: usize,
    rsize: usize,
    chars: String,
    render: String,
}

#[derive(Debug)]
struct EditorConfig {
    cx: u16,
    cy: u16,
    rx: u16,
    rowoff: u16,
    coloff: u16,
    screenrows: u16,
    screencols: u16,
    numrows: u16,
    row: Vec<Erow>,
    filename: String,
    statusmsg: String,
    statusmsg_time: SystemTime,
    orig_termios: RawMode,
}

fn unsafe_write(buf: &str) -> Result<isize, io::Error> {
    let ret = unsafe {
        libc::write(
            libc::STDOUT_FILENO,
            buf.as_ptr() as *const libc::c_void,
            buf.len(),
        )
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
        rx: 0,
        rowoff: 0,
        coloff: 0,
        screenrows: 0,
        screencols: 0,
        numrows: 0,
        row: vec![],
        filename: String::new(),
        statusmsg: String::new(),
        statusmsg_time: SystemTime::now(),
        orig_termios: RawMode(orig_termios),
    }
}

fn editor_read_key() -> EditorKey {
    let mut c = [0u8; 1];

    loop {
        match io::stdin().read_exact(&mut c) {
            Ok(_) => {
                break;
            }
            Err(e) => {
                if e.kind() != io::ErrorKind::UnexpectedEof {
                    die("Read Key Error", e);
                }
            }
        }
    }

    if c[0] == b'\x1b' {
        let mut seq0 = [0u8; 1];
        let mut seq1 = [0u8; 1];
        let mut seq2 = [0u8; 1];
        if io::stdin().read_exact(&mut seq0).is_err() {
            return EditorKey::Esc;
        }
        if io::stdin().read_exact(&mut seq1).is_err() {
            return EditorKey::Esc;
        }

        if seq0[0] == b'[' {
            if seq1[0] >= b'0' && seq1[0] <= b'9' {
                if io::stdin().read_exact(&mut seq2).is_err() {
                    return EditorKey::Esc;
                }
                if seq2[0] == b'~' {
                    match seq1[0] {
                        b'1' => return EditorKey::HomeKey,
                        b'3' => return EditorKey::DelKey,
                        b'4' => return EditorKey::EndKey,
                        b'5' => return EditorKey::PageUp,
                        b'6' => return EditorKey::PageDown,
                        b'7' => return EditorKey::HomeKey,
                        b'8' => return EditorKey::EndKey,
                        _ => {}
                    }
                }
            } else {
                match seq1[0] {
                    b'A' => return EditorKey::ArrowUp,
                    b'B' => return EditorKey::ArrowDown,
                    b'C' => return EditorKey::ArrowRight,
                    b'D' => return EditorKey::ArrowLeft,
                    b'H' => return EditorKey::HomeKey,
                    b'F' => return EditorKey::EndKey,
                    _ => {}
                }
            }
        } else if seq0[0] == b'O' {
            match seq1[0] {
                b'H' => return EditorKey::HomeKey,
                b'F' => return EditorKey::EndKey,
                _ => {}
            }
        }

        return EditorKey::Esc;
    }

    EditorKey::Key(c[0])
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
        return Some((row, col));
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

fn editor_row_cx_to_rx(row: &Erow, cx: u16) -> u16 {
    let mut rx: u16 = 0;
    let data: Vec<char> = row.chars.chars().collect();
    for j in 0..cx as usize {
        if data[j] == '\t' {
            rx += (EDITOR_TAB_STOP - 1) - (rx % EDITOR_TAB_STOP);
        }
        rx += 1;
    }
    rx
}

fn editor_update_row(row: &mut Erow) {
    let mut tabs = 0;

    let data: Vec<char> = row.chars.chars().collect();
    for j in 0..row.size {
        if data[j] == '\t' {
            tabs += 1;
        }
    }

    let mut idx = 0;
    let mut rendered = String::new();
    for j in 0..row.size {
        if data[j] == '\t' {
            rendered.push(' ');
            while idx % EDITOR_TAB_STOP != 0 {
                rendered.push(' ');
            }
        } else {
            rendered.push(data[j]);
        }
    }

    row.render = row.chars.clone();
    row.rsize = row.size;
}

fn editor_append_row(conf: &mut EditorConfig, s: &str) {
    let mut row = Erow {
        size: s.len(),
        chars: String::from(s),
        rsize: 0,
        render: String::new(),
    };
    editor_update_row(&mut row);
    conf.row.push(row);
    conf.numrows += 1;
}

fn editor_open(conf: &mut EditorConfig, filename: &str) {
    conf.filename = String::from(filename);

    let f = File::open(filename).expect("Can't open a file");
    let mut reader = io::BufReader::new(f);
    let mut line = String::new();

    loop {
        let mut linelen = reader.read_line(&mut line).expect("Can't read a line");
        if linelen == 0 {
            return;
        }

        let mut data: Vec<char> = line.chars().collect();

        while linelen > 0 && (data[linelen - 1] == '\n' || data[linelen - 1] == '\r') {
            linelen -= 1;
        }

        editor_append_row(conf, &line[..linelen]);

        line.clear();
    }
}

struct Abuf {
    b: String,
}

impl Abuf {
    fn new() -> Abuf {
        Abuf { b: String::new() }
    }

    fn append(&mut self, s: &str) {
        self.b.push_str(s);
    }
}

fn editor_scroll(conf: &mut EditorConfig) {
    conf.rx = 0;
    if conf.cy < conf.numrows {
        conf.rx = editor_row_cx_to_rx(&conf.row[conf.cy as usize], conf.cx);
    }

    if conf.cy < conf.rowoff {
        conf.rowoff = conf.cy;
    }
    if conf.cy >= conf.rowoff + conf.screenrows {
        conf.rowoff = conf.cy - conf.screenrows + 1;
    }
    if conf.rx < conf.coloff {
        conf.coloff = conf.rx;
    }
    if conf.rx >= conf.coloff + conf.screencols {
        conf.coloff = conf.rx - conf.screencols + 1;
    }
}

fn editor_draw_rows(conf: &EditorConfig, ab: &mut Abuf) {
    for y in 0..conf.screenrows {
        let filerow = y + conf.rowoff;
        if filerow >= conf.numrows {
            if conf.numrows == 0 && y == conf.screenrows / 3 {
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
        } else {
            let mut len = conf.row[filerow as usize].rsize as i16 - conf.coloff as i16;

            if len > 0 {
                if len > conf.screencols as i16 {
                    len = conf.screencols as i16;
                }
                len += conf.coloff as i16;
                ab.append(&conf.row[filerow as usize].render[conf.coloff as usize..len as usize]);
            }
        }

        ab.append("\x1b[K");
        ab.append("\r\n");
    }
}

fn editor_draw_status_bar(conf: &EditorConfig, ab: &mut Abuf) {
    ab.append("\x1b[7m");

    let filename = match conf.filename.is_empty() {
        true => "[No Name]",
        false => &conf.filename,
    };

    let status = format!("{} - {} lines", &filename, conf.numrows);
    let rstatus = format!("{}/{}", conf.cy + 1, conf.numrows);

    let mut len = status.len() as u16;
    let rlen = rstatus.len() as u16;
    if len > conf.screencols {
        len = conf.screencols;
    }
    ab.append(&status[..len as usize]);

    while len < conf.screencols {
        if conf.screencols - len == rlen {
            ab.append(&rstatus);
            break;
        } else {
            ab.append(" ");
            len += 1;
        }
    }

    ab.append("\x1b[m");
    ab.append("\r\n");
}

fn editor_draw_message_bar(conf: &EditorConfig, ab: &mut Abuf) {
    ab.append("\x1b[K");
    let mut msglen = conf.statusmsg.len();
    if msglen > conf.screencols as usize {
        msglen = conf.screencols as usize;
    }
    if msglen > 0 && SystemTime::now().duration_since(conf.statusmsg_time).unwrap() < Duration::new(5, 0) {
        ab.append(&conf.statusmsg);
    }
}

fn editor_refresh_screen(conf: &mut EditorConfig) {
    editor_scroll(conf);

    let mut ab = Abuf::new();

    ab.append("\x1b[?25l");
    ab.append("\x1b[H");

    editor_draw_rows(conf, &mut ab);
    editor_draw_status_bar(conf, &mut ab);
    editor_draw_message_bar(conf, &mut ab);

    let cursor = format!(
        "\x1b[{};{}H",
        (conf.cy - conf.rowoff) + 1,
        (conf.rx - conf.coloff) + 1
    );
    ab.append(cursor.as_str());

    ab.append("\x1b[?25h");

    _ = io::stdout().write(ab.b.as_bytes());
    _ = io::stdout().flush();
}

fn editor_set_status_message(conf: &mut EditorConfig, fmt: &str) {
    conf.statusmsg = fmt.to_string();
}

fn editor_move_cursor(key: EditorKey, conf: &mut EditorConfig) {
    let mut size: Option<usize> = None;
    if conf.cy < conf.numrows {
        size = Some(conf.row[conf.cy as usize].size);
    }

    match key {
        EditorKey::ArrowUp => {
            if conf.cy != 0 {
                conf.cy -= 1;
            }
        }
        EditorKey::ArrowDown => {
            if conf.cy < conf.numrows {
                conf.cy += 1;
            }
        }
        EditorKey::ArrowLeft => {
            if conf.cx != 0 {
                conf.cx -= 1;
            } else if conf.cy > 0 {
                conf.cy -= 1;
                conf.cx = conf.row[conf.cy as usize].size as u16;
            }
        }
        EditorKey::ArrowRight => {
            if let Some(s) = size {
                if conf.cx < s as u16 {
                    conf.cx += 1;
                } else if conf.cx == s as u16 {
                    conf.cy += 1;
                    conf.cx = 0;
                }
            }
        }
        _ => {}
    }

    if conf.cy < conf.numrows {
        if conf.cx > conf.row[conf.cy as usize].size as u16 {
            conf.cx = conf.row[conf.cy as usize].size as u16;
        }
    }
}

fn editor_process_keypress(conf: &mut EditorConfig) -> bool {
    let c = editor_read_key();

    if let EditorKey::Key(key) = c {
        if key == ctrl_key!(b'q') {
            _ = io::stdout().write(b"\x1b[2J");
            _ = io::stdout().write(b"\x1b[H");
            return false;
        }
    }

    match c {
        EditorKey::HomeKey => conf.cx = 0,
        EditorKey::EndKey => if conf.cy < conf.numrows {
            conf.cx = conf.row[conf.cy as usize].size as u16;
        },
        EditorKey::PageUp | EditorKey::PageDown => {
            match c {
                EditorKey::PageUp => conf.cy = conf.rowoff,
                EditorKey::PageDown => {
                    conf.cy = conf.rowoff + conf.screenrows - 1;
                    if conf.cy > conf.numrows {
                        conf.cy = conf.numrows;
                    }
                }
                _ => {}
            }

            let mut times = conf.screenrows;
            while times > 0 {
                times -= 1;
                match c {
                    EditorKey::PageUp => editor_move_cursor(EditorKey::ArrowUp, conf),
                    _ => editor_move_cursor(EditorKey::ArrowDown, conf),
                }
            }
        }
        EditorKey::ArrowUp
        | EditorKey::ArrowDown
        | EditorKey::ArrowLeft
        | EditorKey::ArrowRight => editor_move_cursor(c, conf),
        _ => {}
    }

    true
}

fn init_editor(conf: &mut EditorConfig) -> bool {
    let Some((row, col)) = get_window_size() else {
        return false;
    };

    conf.screenrows = row - 2;
    conf.screencols = col;
    true
}

fn main() {
    let mut conf: EditorConfig = enable_raw_mode();
    if init_editor(&mut conf) == false {
        return;
    }

    let args: Vec<String> = env::args().collect();
    if args.len() >= 2 {
        editor_open(&mut conf, &args[1]);
    }

    editor_set_status_message(&mut conf, "HELP: Ctrl-Q = quit");

    loop {
        editor_refresh_screen(&mut conf);
        if editor_process_keypress(&mut conf) == false {
            break;
        }
    }
}
