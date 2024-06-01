use std::cmp;
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;
use std::str;
use std::time::{Duration, SystemTime};

use cjk::is_cjk_codepoint;
use nix::libc::{self, ioctl, TIOCGWINSZ};
use nix::pty::Winsize;
use termios::*;

const EDITOR_VERSION: &'static str = "0.0.1";
const EDITOR_TAB_STOP: u16 = 8;
const EDITOR_QUIT_TIMES: u16 = 3;

macro_rules! ctrl_key {
    ($k:expr) => {
        $k & 0x1f
    };
}

#[repr(u8)]
enum EditorKey {
    Key(u8),
    Backspace = 127,
    Enter = b'\r',
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

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum EditorHighlight {
    HlNormal = 37,
    HlComment = 36,
    HlMlComment = 38, // -> 36
    HlKeyword1 = 33,
    HlKeyword2 = 32,
    HlString = 35,
    HlNumber = 31,
    HlMatch = 34,
}

const HL_HIGHLIGHT_NUMBERS: u32 = 1 << 0;
const HL_HIGHLIGHT_STRINGS: u32 = 1 << 1;

#[derive(Debug, Clone, Copy)]
struct EditorSyntax<'a> {
    filetype: &'a str,
    filematch: &'a [&'a str],
    keywords: &'a [&'a str],
    singleline_comment_start: &'a str,
    multiline_comment_start: &'a str,
    multiline_comment_end: &'a str,
    flags: u32,
}

#[derive(Debug)]
struct RawMode(Termios);

impl Drop for RawMode {
    fn drop(&mut self) {
        disable_raw_mode(self);
    }
}

#[derive(Debug, Clone)]
struct Erow {
    idx: u16,
    size: usize,
    rsize: usize,
    chars: String,
    render: String,
    hl: Vec<EditorHighlight>,
    hl_open_comment: bool,
}

impl Erow {
    fn new(at: u16, s: &str) -> Erow {
        Erow {
            idx: at,
            size: s.chars().collect::<Vec<char>>().len(),
            chars: String::from(s),
            rsize: 0,
            render: String::new(),
            hl: vec![],
            hl_open_comment: false,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
struct EditorConfig<'a> {
    cx: u16,
    cy: u16,
    rx: u16,
    rowoff: u16,
    coloff: u16,
    screenrows: u16,
    screencols: u16,
    numrows: u16,
    row: Vec<Erow>,
    dirty: u16,
    filename: String,
    statusmsg: String,
    statusmsg_time: SystemTime,
    syntax: Option<EditorSyntax<'a>>,
    orig_termios: RawMode,
    quit_times: u16,
    last_match: i16,
    direction: i16,
    saved_hl_line: i16,
    saved_hl: Vec<EditorHighlight>,
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

static C_HL_EXTENSIONS: &[&str] = &[".c", ".h", ".cpp"];
static C_HL_KEYWORDS: &[&str] = &[
    "switch",
    "if",
    "while",
    "for",
    "break",
    "continue",
    "return",
    "else",
    "struct",
    "union",
    "typedef",
    "static",
    "enum",
    "class",
    "case",
    "int|",
    "long|",
    "double|",
    "float|",
    "char|",
    "unsigned|",
    "signed|",
    "void|",
];

static RUST_HL_EXTENSIONS: &[&str] = &[".rs"];
static RUST_HL_KEYWORDS: &[&str] = &[
    "if",
    "else",
    "while",
    "for",
    "loop",
    "match",
    "break",
    "continue",
    "return",
    "struct",
    "try",
    "catch",
    "as",
    "dyn",
    "macro_rules",
    "macro",
    "enum|",
    "union|",
    "impl|",
    "trait|",
    "const|",
    "static|",
    "let|",
    "mut|",
    "ref|",
    "pub|",
    "crate|",
    "super|",
    "self|",
    "mod|",
    "extern|",
    "async|",
    "await|",
    "use|",
    "bool|",
    "char|",
    "str|",
    "u8|",
    "u16|",
    "u32|",
    "u64|",
    "u128|",
    "i8|",
    "i16|",
    "i32|",
    "i64|",
    "i128|",
    "isize|",
    "usize|",
    "f32|",
    "f64|",
    "fn|",
];

static HLDB: &[EditorSyntax] = &[
    EditorSyntax {
        filetype: "c",
        filematch: C_HL_EXTENSIONS,
        keywords: C_HL_KEYWORDS,
        singleline_comment_start: "//",
        multiline_comment_start: "/*",
        multiline_comment_end: "*/",
        flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
    },
    EditorSyntax {
        filetype: "rust",
        filematch: RUST_HL_EXTENSIONS,
        keywords: RUST_HL_KEYWORDS,
        singleline_comment_start: "//",
        multiline_comment_start: "/*",
        multiline_comment_end: "*/",
        flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
    },
];

#[allow(unused)]
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

fn enable_raw_mode<'a>() -> EditorConfig<'a> {
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
        dirty: 0,
        filename: String::new(),
        statusmsg: String::new(),
        statusmsg_time: SystemTime::now(),
        syntax: None,
        orig_termios: RawMode(orig_termios),
        quit_times: 0,
        last_match: -1,
        direction: 1,
        saved_hl_line: 0,
        saved_hl: vec![],
    }
}

fn editor_row_cx_to_rx(row: &Erow, cx: u16) -> u16 {
    let mut rx: u16 = 0;
    let data: Vec<char> = row.chars.chars().collect();
    for j in 0..cx as usize {
        if data[j] == '\t' {
            rx += (EDITOR_TAB_STOP - 1) - (rx % EDITOR_TAB_STOP);
        } else if is_cjk_codepoint(data[j]) {
            rx += 1;
        }
        rx += 1;
    }
    rx
}

fn editor_row_rx_to_cx(row: &mut Erow, rx: u16) -> u16 {
    let mut cur_rx: u16 = 0;
    let mut cx = row.size as u16;

    let data: Vec<char> = row.chars.chars().collect();
    for i in 0..row.size as u16 {
        cx = i;

        if data[cx as usize] == '\t' {
            cur_rx += (EDITOR_TAB_STOP - 1) - (cur_rx % EDITOR_TAB_STOP);
        } else if is_cjk_codepoint(data[cx as usize]) {
            cur_rx += 1;
        }
        cur_rx += 1;

        if cur_rx > rx {
            return cx;
        }
    }
    cx
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

    if c[0] == b'\r' {
        return EditorKey::Enter;
    }

    if c[0] == 127 {
        return EditorKey::Backspace;
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
        if ioctl(io::stdout().as_raw_fd(), TIOCGWINSZ, &mut ws) == -1 || ws.ws_col == 0 {
            if io::stdout().write(b"\x1b[999C\x1b[999B").is_err() {
                return None;
            }
            return get_cursor_position();
        }
    };

    Some((ws.ws_row, ws.ws_col))
}

fn is_seprator(c: char) -> bool {
    if c.is_whitespace() {
        return true;
    } else if c == '\0' {
        return true;
    }

    match c {
        ',' | '.' | '(' | ')' | '+' | '-' | '/' | '*' | '=' | '~' | '%' | '<' | '>' | '[' | ']'
        | '"' => true,
        _ => false,
    }
}

impl<'a> EditorConfig<'a> {
    fn update_syntax(&mut self, at: u16) {
        let idx = self.row[at as usize].idx as usize;
        let hl_open_comment = match idx > 0 {
            true => self.row[idx - 1].hl_open_comment,
            false => false,
        };
        let row = &mut self.row[at as usize];

        let syntax = self.syntax;
        row.hl = vec![EditorHighlight::HlNormal; row.rsize];
        let data: Vec<char> = row.render.chars().collect();

        let (
            flags,
            keywords,
            singleline_comment_start,
            multiline_comment_start,
            multiline_comment_end,
        ) = match syntax {
            Some(x) => (
                x.flags,
                x.keywords,
                x.singleline_comment_start,
                x.multiline_comment_start,
                x.multiline_comment_end,
            ),
            None => return,
        };

        let scs = singleline_comment_start.chars().collect::<Vec<char>>();
        let mcs = multiline_comment_start.chars().collect::<Vec<char>>();
        let mce = multiline_comment_end.chars().collect::<Vec<char>>();

        let mut prev_sep = true;
        let mut is_in_comment = hl_open_comment;
        let mut is_in_string = false;
        let mut in_string: char = '\0';
        let mut i = 0;

        while i < row.rsize {
            let c = data[i];

            let prev_hl = match i > 0 {
                true => row.hl[i - 1],
                false => EditorHighlight::HlNormal,
            };

            if !scs.is_empty() && !is_in_string && !is_in_comment {
                if i + scs.len() <= data.len() && data[i..i + scs.len()] == scs {
                    (i..row.rsize).for_each(|idx| {
                        row.hl[idx] = EditorHighlight::HlComment;
                    });
                    break;
                }
            }

            if !mcs.is_empty() && !mce.is_empty() && !is_in_string {
                if is_in_comment {
                    row.hl[i] = EditorHighlight::HlMlComment;
                    if i + mce.len() <= data.len() && data[i..i + mce.len()] == mce {
                        (i..i + mce.len()).for_each(|idx| {
                            row.hl[idx] = EditorHighlight::HlMlComment;
                        });
                        i += mce.len();
                        is_in_comment = false;
                        prev_sep = true;
                        continue;
                    } else {
                        i += 1;
                        continue;
                    }
                } else if i + mcs.len() <= data.len() && data[i..i + mcs.len()] == mcs {
                    (i..i + mcs.len()).for_each(|idx| {
                        row.hl[idx] = EditorHighlight::HlMlComment;
                    });
                    i += mcs.len();
                    is_in_comment = true;
                    continue;
                }
            }

            if flags & HL_HIGHLIGHT_STRINGS > 0 {
                if is_in_string {
                    row.hl[i] = EditorHighlight::HlString;
                    if c == '\\' && i + 1 < row.rsize {
                        row.hl[i + 1] = EditorHighlight::HlString;
                        i += 2;
                        continue;
                    }
                    if c == in_string {
                        is_in_string = false;
                    }
                    i += 1;
                    prev_sep = true;
                    continue;
                } else {
                    if c == '\'' && (i > 0 && (data[i - 1] == '&' || data[i - 1] == '<')) {
                        i += 1;
                        continue;
                    }

                    if c == '"' || c == '\'' {
                        in_string = c;
                        is_in_string = true;
                        row.hl[i] = EditorHighlight::HlString;
                        i += 1;
                        continue;
                    }
                }
            }

            if flags & HL_HIGHLIGHT_NUMBERS > 0 {
                if c.is_digit(10)
                    && (prev_sep == true || matches!(prev_hl, EditorHighlight::HlNumber))
                    || (c == '.' && matches!(prev_hl, EditorHighlight::HlNumber))
                {
                    row.hl[i] = EditorHighlight::HlNumber;
                    i += 1;
                    prev_sep = false;
                    continue;
                }
            }

            if prev_sep {
                let mut k = 0;
                for _ in 0..keywords.len() {
                    let kw = keywords[k].chars().collect::<Vec<char>>();
                    let mut klen = kw.len();
                    let kw2 = *kw.last().unwrap() == '|';
                    if kw2 {
                        klen -= 1;
                    }

                    if i + klen < data.len()
                        && data[i..i + klen] == kw[..klen]
                        && is_seprator(data[i + klen])
                    {
                        (i..i + klen).for_each(|idx| {
                            row.hl[idx] = if kw2 {
                                EditorHighlight::HlKeyword2
                            } else {
                                EditorHighlight::HlKeyword1
                            };
                        });
                        i += klen;
                        break;
                    }
                    k += 1;
                }
                if k != keywords.len() {
                    prev_sep = false;
                    continue;
                }
            }

            prev_sep = is_seprator(c);
            i += 1;
        }

        let changed = row.hl_open_comment != is_in_comment;
        row.hl_open_comment = is_in_comment;
        let next = row.idx + 1;
        if changed && next < self.numrows {
            self.update_syntax(next);
        }
    }

    fn select_syntax_highlight(&mut self) {
        self.syntax = None;
        if self.filename.is_empty() {
            return;
        }

        let extension = self
            .filename
            .rfind('.')
            .map(|index| self.filename[index..].to_owned());

        match extension {
            Some(ext) => {
                for entry in HLDB {
                    if entry.filematch.contains(&ext.as_str()) {
                        self.syntax = Some(*entry);

                        for filerow in 0..self.numrows {
                            self.update_syntax(filerow);
                        }
                    }
                }
            }
            None => {}
        }
    }

    fn update_row(&mut self, at: u16) {
        let row: &mut Erow = &mut self.row[at as usize];

        let mut _tabs = 0;

        let data: Vec<char> = row.chars.chars().collect();
        for j in 0..data.len() {
            if data[j] == '\t' {
                _tabs += 1;
            }
        }

        let mut idx = 0;
        let mut rendered = String::new();
        for j in 0..data.len() {
            if data[j] == '\t' {
                rendered.push(' ');
                idx += 1;
                while idx % EDITOR_TAB_STOP != 0 {
                    rendered.push(' ');
                    idx += 1;
                }
            } else {
                rendered.push(data[j]);
                idx += 1;
            }
        }

        row.render = rendered.clone();
        row.rsize = rendered.chars().collect::<Vec<char>>().len();

        self.update_syntax(at);
    }

    fn insert_row(&mut self, at: u16, s: &str) {
        if at > self.numrows {
            return;
        }

        for j in at + 1..self.numrows {
            self.row[j as usize].idx += 1;
        }

        self.row.insert(at.into(), Erow::new(at, s));
        self.update_row(at);
        self.numrows += 1;
        self.dirty += 1;
    }

    fn del_row(&mut self, at: u16) {
        if at >= self.numrows {
            return;
        }
        self.row.remove(at.into());

        for j in at..self.numrows - 1 {
            self.row[j as usize].idx -= 1;
        }

        self.numrows -= 1;
        self.dirty += 1;
    }

    fn row_insert_char(&mut self, at: u16, c: u8) {
        let row: &mut Erow = &mut self.row[at as usize];

        let mut index: usize = self.cx.into();
        if self.cx as usize > row.size {
            index = row.size;
        }

        let mut s = row.chars.chars().collect::<Vec<_>>();
        s.insert(index, c as char);

        row.size += 1;
        row.chars = s.into_iter().collect::<String>();
        self.update_row(at);
        self.dirty += 1;
    }

    fn row_append_string(&mut self, at: u16, s: &str) {
        let row: &mut Erow = &mut self.row[at as usize];

        row.chars.push_str(s);
        row.size += s.len();

        self.update_row(at);
        self.dirty += 1;
    }

    fn row_del_char(&mut self, at: u16) {
        let row: &mut Erow = &mut self.row[at as usize];

        if self.cx as usize >= row.size {
            return;
        }

        row.size -= 1;
        row.chars.remove(self.cx.into());

        self.update_row(at);
        self.dirty += 1;
    }

    fn insert_char(&mut self, c: u8) {
        if self.cy == self.numrows {
            self.insert_row(self.numrows, "");
        }

        self.row_insert_char(self.cy, c);
        self.cx += 1;
    }

    fn insert_newline(&mut self) {
        if self.cx == 0 {
            self.insert_row(self.cy, "");
        } else {
            let cx = self.cx;
            let cy = self.cy;

            let s = self.row[cy as usize].chars.chars().collect::<Vec<char>>();
            self.insert_row(cy + 1, &s[cx as usize..].iter().collect::<String>());

            let row = &mut self.row[cy as usize];
            row.chars = s[..cx as usize].iter().collect::<String>();
            row.size = cx as usize;

            self.update_row(cy);
        }
        self.cy += 1;
        self.cx = 0;
    }

    fn del_char(&mut self) {
        if self.cy == self.numrows {
            return;
        }

        if self.cx == 0 && self.cy == 0 {
            return;
        }

        if self.cx > 0 {
            self.cx -= 1;
            self.row_del_char(self.cy);
        } else {
            self.cx = self.row[self.cy as usize - 1].size as u16;
            self.row_append_string(self.cy - 1, &self.row[self.cy as usize].chars.clone());
            self.del_row(self.cy);
            self.cy -= 1;
        }
    }

    fn rows_to_string(&self) -> String {
        self.row
            .iter()
            .map(|erow| erow.chars.as_str())
            .collect::<Vec<&str>>()
            .join("\n")
    }

    fn open(&mut self, filename: &str) {
        self.filename = String::from(filename);

        self.select_syntax_highlight();

        let f = File::open(filename).expect("Can't open a file");
        let mut reader = io::BufReader::new(f);
        let mut line: Vec<char> = vec![];
        let mut window: Vec<u8> = vec![];
        let mut buffer = [0; 1];

        while reader.read(&mut buffer).expect("Can't read a file") > 0 {
            let byte = buffer[0];

            let c = byte as char;

            if c.is_ascii() {
                for _ in 0..window.len() {
                    line.push('?');
                }
                window.clear();

                if c == '\n' || c == '\r' {
                    let string = line.clone().into_iter().collect::<String>();
                    self.insert_row(self.numrows, &string);
                    line.clear();
                } else {
                    line.push(c);
                }

                continue;
            }

            window.push(byte);

            if let Ok(s) = std::str::from_utf8(&window) {
                line.push(s.chars().next().unwrap());
                window.clear();
            }
        }

        for _ in 0..window.len() {
            line.push('?');
        }

        if !line.is_empty() {
            let string = line.clone().into_iter().collect::<String>();
            self.insert_row(self.numrows, &string);
        }

        self.dirty = 0;
    }

    fn save(&mut self) {
        if self.filename.is_empty() {
            if let Some(filename) = self.prompt("Save as: {} (ESC to cancel)", None) {
                self.filename = filename;
            } else {
                self.set_status_message("Save aborted");
                return;
            }

            self.select_syntax_highlight();
        }

        let buf = self.rows_to_string();

        let result = OpenOptions::new()
            .read(true)
            .write(true)
            .truncate(true)
            .create(true)
            .open(&self.filename);

        let (reset_dirty, message) = match result {
            Ok(mut f) => match f.write_all(buf.as_bytes()) {
                Ok(_) => (true, format!("{} bytes written to disk", buf.len())),
                Err(e) => (false, format!("Can't save! I/O error: {}", e.to_string())),
            },
            Err(e) => (false, format!("Can't save! I/O error: {}", e.to_string())),
        };

        if reset_dirty {
            self.dirty = 0;
        }

        self.set_status_message(&message);
    }

    fn find_callback(&mut self, query: &str, c: EditorKey) {
        if !self.saved_hl.is_empty() {
            self.row[self.saved_hl_line as usize].hl = self.saved_hl.clone();
            self.saved_hl = vec![];
        }

        if matches!(c, EditorKey::Enter | EditorKey::Esc) {
            self.last_match = -1;
            self.direction = 1;
            return;
        } else if matches!(c, EditorKey::ArrowRight | EditorKey::ArrowDown) {
            self.direction = 1;
        } else if matches!(c, EditorKey::ArrowLeft | EditorKey::ArrowUp) {
            self.direction = -1;
        } else {
            self.last_match = -1;
            self.direction = 1;
        }

        if self.last_match == -1 {
            self.direction = 1;
        }
        let mut current = self.last_match;

        for _ in 0..self.numrows {
            current += self.direction;
            if current == -1 {
                current = self.numrows as i16 - 1;
            } else if current == self.numrows as i16 {
                current = 0;
            }

            let row = &mut self.row[current as usize];
            if let Some(index) = row.render.find(&query) {
                self.last_match = current;
                self.cy = current as u16;
                self.cx = editor_row_rx_to_cx(row, index as u16);
                self.rowoff = self.numrows;

                self.saved_hl_line = current;
                self.saved_hl = row.hl.clone();
                for j in index..index + query.len() {
                    row.hl[j] = EditorHighlight::HlMatch;
                }
                break;
            }
        }
    }

    fn find(&mut self) {
        let saved_cx = self.cx;
        let saved_cy = self.cy;
        let saved_coloff = self.coloff;
        let saved_rowoff = self.rowoff;

        let query = self.prompt(
            "Search: {} (Use ESC/Arrows/Enter)",
            Some(EditorConfig::find_callback),
        );

        if query.is_none() {
            self.cx = saved_cx;
            self.cy = saved_cy;
            self.coloff = saved_coloff;
            self.rowoff = saved_rowoff;
        }
    }

    fn scroll(&mut self) {
        self.rx = 0;
        if self.cy < self.numrows {
            self.rx = editor_row_cx_to_rx(&self.row[self.cy as usize], self.cx);
        }

        if self.cy < self.rowoff {
            self.rowoff = self.cy;
        }
        if self.cy >= self.rowoff + self.screenrows {
            self.rowoff = self.cy - self.screenrows + 1;
        }
        if self.rx < self.coloff {
            self.coloff = self.rx;
        }
        if self.rx >= self.coloff + self.screencols {
            self.coloff = self.rx - self.screencols + 1;
        }
    }

    fn draw_rows(&self, ab: &mut Abuf) {
        for y in 0..self.screenrows {
            let filerow = y + self.rowoff;
            if filerow >= self.numrows {
                if self.numrows == 0 && y == self.screenrows / 3 {
                    let welcome = format!("The Editor -- version {}", EDITOR_VERSION);
                    let welcomelen = cmp::min(welcome.len(), self.screencols.into());
                    let mut padding = (self.screencols - welcomelen as u16) / 2;
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
                let mut len = self.row[filerow as usize].rsize as i16 - self.coloff as i16;

                if len < 0 {
                    len = 0;
                }
                if len > self.screencols as i16 {
                    len = self.screencols as i16;
                }

                let mut current_color = -1;

                for j in 0..len as u16 {
                    let index = (self.coloff + j) as usize;

                    let c = &self.row[filerow as usize]
                        .render
                        .chars()
                        .nth(index)
                        .unwrap_or('?');

                    if c.is_control() {
                        let sym = if c <= &'\x1A' {
                            char::from_u32((0x40 << 8) | *c as u32).unwrap()
                        } else {
                            '?'
                        };

                        ab.append("\x1b[7m");
                        ab.append(&sym.to_string());
                        ab.append("\x1b[m");
                        if current_color != -1 {
                            ab.append(&format!("\x1b[{}m", current_color));
                        }
                    }

                    let hl = self.row[filerow as usize].hl[index];
                    match hl {
                        EditorHighlight::HlNormal => {
                            if current_color != -1 {
                                ab.append("\x1b[39m");
                                current_color = -1;
                            }
                            ab.append(&c.to_string());
                        }
                        _ => {
                            let color = match hl {
                                EditorHighlight::HlNumber => EditorHighlight::HlNumber,
                                EditorHighlight::HlMatch => EditorHighlight::HlMatch,
                                EditorHighlight::HlString => EditorHighlight::HlString,
                                EditorHighlight::HlComment => EditorHighlight::HlComment,
                                EditorHighlight::HlMlComment => EditorHighlight::HlComment,
                                EditorHighlight::HlKeyword1 => EditorHighlight::HlKeyword1,
                                EditorHighlight::HlKeyword2 => EditorHighlight::HlKeyword2,
                                _ => unreachable!(),
                            } as i8;
                            if color != current_color {
                                current_color = color;
                                ab.append(&format!("\x1b[{}m", color));
                            }
                            ab.append(&c.to_string());
                        }
                    }
                }
                ab.append("\x1b[39m");
            }

            ab.append("\x1b[K");
            ab.append("\r\n");
        }
    }

    fn draw_status_bar(&self, ab: &mut Abuf) {
        ab.append("\x1b[7m");

        let filename = match self.filename.is_empty() {
            true => "[No Name]",
            false => &self.filename,
        };

        let modified = match self.dirty > 0 {
            true => "(modified)",
            false => "",
        };

        let syntax = match &self.syntax {
            Some(x) => x.filetype,
            None => "no ft",
        };

        let status = format!("{} - {} lines {}", &filename, self.numrows, modified);
        let rstatus = format!("{} | {}/{}", syntax, self.cy + 1, self.numrows);

        let mut len = status.len() as u16;
        let rlen = rstatus.len() as u16;
        if len > self.screencols {
            len = self.screencols;
        }
        ab.append(&status[..len as usize]);

        while len < self.screencols {
            if self.screencols - len == rlen {
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

    fn draw_message_bar(&self, ab: &mut Abuf) {
        ab.append("\x1b[K");
        let mut msglen = self.statusmsg.len();
        if msglen > self.screencols as usize {
            msglen = self.screencols as usize;
        }
        if msglen > 0
            && SystemTime::now()
                .duration_since(self.statusmsg_time)
                .unwrap()
                < Duration::new(5, 0)
        {
            ab.append(&self.statusmsg);
        }
    }

    fn refresh_screen(&mut self) {
        self.scroll();

        let mut ab = Abuf::new();

        ab.append("\x1b[?25l");
        ab.append("\x1b[H");

        self.draw_rows(&mut ab);
        self.draw_status_bar(&mut ab);
        self.draw_message_bar(&mut ab);

        let cursor = format!(
            "\x1b[{};{}H",
            (self.cy - self.rowoff) + 1,
            (self.rx - self.coloff) + 1
        );
        ab.append(cursor.as_str());

        ab.append("\x1b[?25h");

        _ = io::stdout().write(ab.b.as_bytes());
        _ = io::stdout().flush();
    }

    fn set_status_message(&mut self, fmt: &str) {
        self.statusmsg = fmt.to_string();
        self.statusmsg_time = SystemTime::now();
    }

    fn prompt(
        &mut self,
        prompt: &str,
        callback: Option<fn(&mut EditorConfig<'a>, &str, EditorKey)>,
    ) -> Option<String> {
        let mut buf = String::new();

        loop {
            self.set_status_message(String::from(prompt).replace("{}", buf.as_str()).as_str());
            self.refresh_screen();

            let c = editor_read_key();
            match c {
                EditorKey::Backspace | EditorKey::DelKey => {
                    if !buf.is_empty() {
                        buf.pop();
                    }
                }
                EditorKey::Esc => {
                    self.set_status_message("");
                    callback.as_ref().map(|cb| cb(self, &buf, c));
                    return None;
                }
                EditorKey::Enter => {
                    if !buf.is_empty() {
                        self.set_status_message("");
                        callback.as_ref().map(|cb| cb(self, &buf, c));
                        return Some(buf);
                    }
                }
                EditorKey::Key(key) => {
                    if !buf.is_empty() && key == ctrl_key!(b'h') {
                        buf.pop();
                    } else if !key.is_ascii_control() && key < 128 {
                        buf.push(key as char);
                    }
                }
                _ => {}
            }

            callback.as_ref().map(|cb| cb(self, &buf, c));
        }
    }

    fn move_cursor(&mut self, key: EditorKey) {
        let mut size: Option<usize> = None;
        if self.cy < self.numrows {
            size = Some(self.row[self.cy as usize].size);
        }

        match key {
            EditorKey::ArrowUp => {
                if self.cy != 0 {
                    self.cy -= 1;
                }
            }
            EditorKey::ArrowDown => {
                if self.cy < self.numrows {
                    self.cy += 1;
                }
            }
            EditorKey::ArrowLeft => {
                if self.cx != 0 {
                    self.cx -= 1;
                } else if self.cy > 0 {
                    self.cy -= 1;
                    self.cx = self.row[self.cy as usize].size as u16;
                }
            }
            EditorKey::ArrowRight => {
                if let Some(s) = size {
                    if self.cx < s as u16 {
                        self.cx += 1;
                    } else if self.cx == s as u16 {
                        self.cy += 1;
                        self.cx = 0;
                    }
                }
            }
            _ => {}
        }

        if self.cy < self.numrows {
            if self.cx > self.row[self.cy as usize].size as u16 {
                self.cx = self.row[self.cy as usize].size as u16;
            }
        }
    }

    fn process_keypress(&mut self) -> bool {
        let c = editor_read_key();

        match c {
            EditorKey::Key(key) => {
                if key == ctrl_key!(b'q') {
                    if self.dirty > 0 && self.quit_times > 0 {
                        let message = format!(
                        "WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit.",
                        self.quit_times
                    );
                        self.set_status_message(&message);
                        self.quit_times -= 1;
                        return true;
                    }
                    _ = io::stdout().write(b"\x1b[2J");
                    _ = io::stdout().write(b"\x1b[H");
                    return false;
                } else if key == ctrl_key!(b'h') {
                    // backspace
                    self.del_char();
                } else if key == ctrl_key!(b'l') { // escape
                     // pass
                } else if key == ctrl_key!(b's') {
                    self.save();
                } else if key == ctrl_key!(b'f') {
                    self.find();
                } else {
                    self.insert_char(key);
                }
            }
            EditorKey::HomeKey => self.cx = 0,
            EditorKey::EndKey => {
                if self.cy < self.numrows {
                    self.cx = self.row[self.cy as usize].size as u16;
                }
            }
            EditorKey::Backspace => {
                self.del_char();
            }
            EditorKey::DelKey => {
                self.move_cursor(EditorKey::ArrowRight);
                self.del_char();
            }
            EditorKey::PageUp | EditorKey::PageDown => {
                match c {
                    EditorKey::PageUp => self.cy = self.rowoff,
                    EditorKey::PageDown => {
                        self.cy = self.rowoff + self.screenrows - 1;
                        if self.cy > self.numrows {
                            self.cy = self.numrows;
                        }
                    }
                    _ => {}
                }

                let mut times = self.screenrows;
                while times > 0 {
                    times -= 1;
                    match c {
                        EditorKey::PageUp => self.move_cursor(EditorKey::ArrowUp),
                        _ => self.move_cursor(EditorKey::ArrowDown),
                    }
                }
            }
            EditorKey::ArrowUp
            | EditorKey::ArrowDown
            | EditorKey::ArrowLeft
            | EditorKey::ArrowRight => self.move_cursor(c),
            EditorKey::Enter => self.insert_newline(),
            EditorKey::Esc => {}
        }

        self.quit_times = EDITOR_QUIT_TIMES;

        true
    }

    fn init(&mut self) -> bool {
        let Some((row, col)) = get_window_size() else {
            return false;
        };

        self.screenrows = row - 2;
        self.screencols = col;
        true
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut editor = enable_raw_mode();

    if !editor.init() {
        return;
    }

    if let Some(filename) = args.get(1) {
        editor.open(filename);
    }

    editor.set_status_message("도움말: Ctrl-S = 저장 | Ctrl-Q = 종료 | Ctrl-F = 검색");

    while {
        editor.refresh_screen();
        editor.process_keypress()
    } {}
}
