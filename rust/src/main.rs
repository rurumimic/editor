use std::cmp;
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;
use std::str;
use std::time::{Duration, SystemTime};

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

#[derive(Debug)]
struct Erow {
    idx: u16,
    size: usize,
    rsize: usize,
    chars: String,
    render: String,
    hl: Vec<EditorHighlight>,
    hl_open_comment: bool,
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

static HLDB: &[EditorSyntax] = &[EditorSyntax {
    filetype: "c",
    filematch: C_HL_EXTENSIONS,
    keywords: C_HL_KEYWORDS,
    singleline_comment_start: "//",
    multiline_comment_start: "/*",
    multiline_comment_end: "*/",
    flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
}];

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

// #[allow(unused_assignments)]
fn editor_update_syntax(conf: &mut EditorConfig, at: u16) {
    let idx = conf.row[at as usize].idx as usize;
    let hl_open_comment = conf.row[idx - 1].hl_open_comment;
    let row = &mut conf.row[at as usize];

    let syntax = conf.syntax;
    row.hl = vec![EditorHighlight::HlNormal; row.rsize];
    let data: Vec<char> = row.render.chars().collect();

    let (flags, keywords, singleline_comment_start, multiline_comment_start, multiline_comment_end) =
        match syntax {
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
    let mut is_in_comment = row.idx > 0 && hl_open_comment;
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
            if c.is_digit(10) && (prev_sep == true || matches!(prev_hl, EditorHighlight::HlNumber))
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
    if changed && next < conf.numrows {
        editor_update_syntax(conf, next);
    }
}

fn editor_select_syntax_highlight(conf: &mut EditorConfig) {
    conf.syntax = None;
    if conf.filename.is_empty() {
        return;
    }

    let extension = conf
        .filename
        .rfind('.')
        .map(|index| conf.filename[index..].to_owned());

    match extension {
        Some(ext) => {
            for entry in HLDB {
                if entry.filematch.contains(&ext.as_str()) {
                    conf.syntax = Some(*entry);

                    for filerow in 0..conf.numrows {
                        editor_update_syntax(conf, filerow);
                    }
                }
            }
        }
        None => {}
    }
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

fn editor_row_rx_to_cx(row: &mut Erow, rx: u16) -> u16 {
    let mut cur_rx: u16 = 0;
    let mut cx = row.size as u16;

    let data: Vec<char> = row.chars.chars().collect();
    for i in 0..row.size as u16 {
        cx = i;

        if data[cx as usize] == '\t' {
            cur_rx += (EDITOR_TAB_STOP - 1) - (cur_rx % EDITOR_TAB_STOP);
        }
        cur_rx += 1;

        if cur_rx > rx {
            return cx;
        }
    }
    cx
}

fn editor_update_row(conf: &mut EditorConfig, at: u16, row: &mut Erow) {
    let mut _tabs = 0;

    let data: Vec<char> = row.chars.chars().collect();
    for j in 0..row.size {
        if data[j] == '\t' {
            _tabs += 1;
        }
    }

    let mut idx = 0;
    let mut rendered = String::new();
    for j in 0..row.size {
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

    row.render = row.chars.clone();
    row.rsize = row.size;

    editor_update_syntax(conf, at);
}

fn editor_insert_row(conf: &mut EditorConfig, at: u16, s: &str) {
    if at > conf.numrows {
        return;
    }

    for j in at + 1..conf.numrows {
        conf.row[j as usize].idx += 1;
    }

    let mut row = Erow {
        idx: at,
        size: s.len(),
        chars: String::from(s),
        rsize: 0,
        render: String::new(),
        hl: vec![],
        hl_open_comment: false,
    };

    editor_update_row(conf, at, &mut row);
    conf.row.insert(at.into(), row);
    conf.numrows += 1;
    conf.dirty += 1;
}

fn editor_del_row(conf: &mut EditorConfig, at: u16) {
    if at >= conf.numrows {
        return;
    }
    conf.row.remove(at.into());

    for j in at..conf.numrows - 1 {
        conf.row[j as usize].idx -= 1;
    }

    conf.numrows -= 1;
    conf.dirty += 1;
}

fn editor_row_insert_char(conf: &mut EditorConfig, at: u16, c: u8) {
    let row: &mut Erow = &mut conf.row[at as usize];

    let mut index: usize = conf.cx.into();
    if conf.cx as usize > row.size {
        index = row.size;
    }

    row.size += 1;
    row.chars.insert(index, c as char);
    editor_update_row(conf, at, row);
    conf.dirty += 1;
}

fn editor_row_append_string(conf: &mut EditorConfig, at: u16, s: &str) {
    let row: &mut Erow = &mut conf.row[at as usize];

    row.chars.push_str(s);
    row.size += s.len();

    editor_update_row(conf, at, row);
    conf.dirty += 1;
}

fn editor_row_del_char(conf: &mut EditorConfig, at: u16) {
    let row: &mut Erow = &mut conf.row[at as usize];

    if conf.cx as usize >= row.size {
        return;
    }

    row.size -= 1;
    row.chars.remove(conf.cx.into());
    editor_update_row(conf, at, row);
    conf.dirty += 1;
}

fn editor_insert_char(conf: &mut EditorConfig, c: u8) {
    if conf.cy == conf.numrows {
        editor_insert_row(conf, conf.numrows, "");
    }

    editor_row_insert_char(conf, conf.cy, c);
    conf.cx += 1;
}

fn editor_insert_newline(conf: &mut EditorConfig) {
    if conf.cx == 0 {
        editor_insert_row(conf, conf.cy, "");
    } else {
        let cx = conf.cx;
        let cy = conf.cy;

        let s = conf.row[cy as usize].chars.clone();
        editor_insert_row(conf, cy + 1, &s[cx as usize..]);

        let row = &mut conf.row[cy as usize];
        row.chars = row.chars[..cx as usize].to_string();
        row.size = cx as usize;
        editor_update_row(conf, cy, row);
    }
    conf.cy += 1;
    conf.cx = 0;
}

fn editor_del_char(conf: &mut EditorConfig) {
    if conf.cy == conf.numrows {
        return;
    }

    if conf.cx == 0 && conf.cy == 0 {
        return;
    }

    if conf.cx > 0 {
        conf.cx -= 1;
        editor_row_del_char(conf, conf.cy);
    } else {
        conf.cx = conf.row[conf.cy as usize - 1].size as u16;
        editor_row_append_string(conf, conf.cy - 1, &conf.row[conf.cy as usize].chars.clone());
        editor_del_row(conf, conf.cy);
        conf.cy -= 1;
    }
}

fn editor_rows_to_string(conf: &EditorConfig) -> String {
    conf.row
        .iter()
        .map(|erow| erow.chars.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
}

fn editor_open(conf: &mut EditorConfig, filename: &str) {
    conf.filename = String::from(filename);

    editor_select_syntax_highlight(conf);

    let f = File::open(filename).expect("Can't open a file");
    let mut reader = io::BufReader::new(f);
    let mut line: Vec<char> = vec![];
    let mut buffer = [0; 1];

    while reader.read(&mut buffer).expect("Can't read a file") > 0 {
        let byte = buffer[0] as char;

        if !byte.is_ascii() {
            panic!("This is not a ASCII character: {:?}", byte);
        }

        if byte == '\n' || byte == '\r' {
            let string = line.clone().into_iter().collect::<String>();
            editor_insert_row(conf, conf.numrows, &string);
            line.clear();
            continue;
        }

        line.push(byte);
    }

    if line.len() > 0 {
        let string = line.clone().into_iter().collect::<String>();
        editor_insert_row(conf, conf.numrows, &string);
    }

    conf.dirty = 0;
}

fn editor_save(conf: &mut EditorConfig) {
    if conf.filename.is_empty() {
        if let Some(filename) = editor_prompt(conf, "Save as: {} (ESC to cancel)", None) {
            conf.filename = filename;
        } else {
            editor_set_status_message(conf, "Save aborted");
            return;
        }

        editor_select_syntax_highlight(conf);
    }

    let buf = editor_rows_to_string(conf);

    let result = OpenOptions::new()
        .read(true)
        .write(true)
        .truncate(true)
        .create(true)
        .open(&conf.filename);

    let (reset_dirty, message) = match result {
        Ok(mut f) => match f.write_all(buf.as_bytes()) {
            Ok(_) => (true, format!("{} bytes written to disk", buf.len())),
            Err(e) => (false, format!("Can't save! I/O error: {}", e.to_string())),
        },
        Err(e) => (false, format!("Can't save! I/O error: {}", e.to_string())),
    };

    if reset_dirty {
        conf.dirty = 0;
    }

    editor_set_status_message(conf, &message);
}

fn editor_find_callback(conf: &mut EditorConfig, query: &str, c: EditorKey) {
    if !conf.saved_hl.is_empty() {
        conf.row[conf.saved_hl_line as usize].hl = conf.saved_hl.clone();
        conf.saved_hl = vec![];
    }

    if matches!(c, EditorKey::Enter | EditorKey::Esc) {
        conf.last_match = -1;
        conf.direction = 1;
        return;
    } else if matches!(c, EditorKey::ArrowRight | EditorKey::ArrowDown) {
        conf.direction = 1;
    } else if matches!(c, EditorKey::ArrowLeft | EditorKey::ArrowUp) {
        conf.direction = -1;
    } else {
        conf.last_match = -1;
        conf.direction = 1;
    }

    if conf.last_match == -1 {
        conf.direction = 1;
    }
    let mut current = conf.last_match;

    for _ in 0..conf.numrows {
        current += conf.direction;
        if current == -1 {
            current = conf.numrows as i16 - 1;
        } else if current == conf.numrows as i16 {
            current = 0;
        }

        let row = &mut conf.row[current as usize];
        if let Some(index) = row.render.find(&query) {
            conf.last_match = current;
            conf.cy = current as u16;
            conf.cx = editor_row_rx_to_cx(row, index as u16);
            conf.rowoff = conf.numrows;

            conf.saved_hl_line = current;
            conf.saved_hl = row.hl.clone();
            for j in index..index + query.len() {
                row.hl[j] = EditorHighlight::HlMatch;
            }
            break;
        }
    }
}

fn editor_find(conf: &mut EditorConfig) {
    let saved_cx = conf.cx;
    let saved_cy = conf.cy;
    let saved_coloff = conf.coloff;
    let saved_rowoff = conf.rowoff;

    let query = editor_prompt(
        conf,
        "Search: {} (Use ESC/Arrows/Enter)",
        Some(editor_find_callback),
    );

    if query.is_none() {
        conf.cx = saved_cx;
        conf.cy = saved_cy;
        conf.coloff = saved_coloff;
        conf.rowoff = saved_rowoff;
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

            if len < 0 {
                len = 0;
            }
            if len > conf.screencols as i16 {
                len = conf.screencols as i16;
            }

            let mut current_color = -1;

            for j in 0..len as u16 {
                let index = (conf.coloff + j) as usize;
                let c = &conf.row[filerow as usize].render[index..]
                    .chars()
                    .nth(0)
                    .unwrap();

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

                let hl = conf.row[filerow as usize].hl[index];
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

fn editor_draw_status_bar(conf: &EditorConfig, ab: &mut Abuf) {
    ab.append("\x1b[7m");

    let filename = match conf.filename.is_empty() {
        true => "[No Name]",
        false => &conf.filename,
    };

    let modified = match conf.dirty > 0 {
        true => "(modified)",
        false => "",
    };

    let syntax = match &conf.syntax {
        Some(x) => x.filetype,
        None => "no ft",
    };

    let status = format!("{} - {} lines {}", &filename, conf.numrows, modified);
    let rstatus = format!("{} | {}/{}", syntax, conf.cy + 1, conf.numrows);

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
    if msglen > 0
        && SystemTime::now()
            .duration_since(conf.statusmsg_time)
            .unwrap()
            < Duration::new(5, 0)
    {
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
    conf.statusmsg_time = SystemTime::now();
}

fn editor_prompt(
    conf: &mut EditorConfig,
    prompt: &str,
    callback: Option<fn(&mut EditorConfig, &str, EditorKey)>,
) -> Option<String> {
    let mut buf = String::new();

    loop {
        editor_set_status_message(
            conf,
            String::from(prompt).replace("{}", buf.as_str()).as_str(),
        );
        editor_refresh_screen(conf);

        let c = editor_read_key();
        match c {
            EditorKey::Backspace | EditorKey::DelKey => {
                if !buf.is_empty() {
                    buf.pop();
                }
            }
            EditorKey::Esc => {
                editor_set_status_message(conf, "");
                callback.as_ref().map(|cb| cb(conf, &buf, c));
                return None;
            }
            EditorKey::Enter => {
                if !buf.is_empty() {
                    editor_set_status_message(conf, "");
                    callback.as_ref().map(|cb| cb(conf, &buf, c));
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

        callback.as_ref().map(|cb| cb(conf, &buf, c));
    }
}

fn editor_move_cursor(conf: &mut EditorConfig, key: EditorKey) {
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

    match c {
        EditorKey::Key(key) => {
            if key == ctrl_key!(b'q') {
                if conf.dirty > 0 && conf.quit_times > 0 {
                    let message = format!(
                        "WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit.",
                        conf.quit_times
                    );
                    editor_set_status_message(conf, &message);
                    conf.quit_times -= 1;
                    return true;
                }
                _ = io::stdout().write(b"\x1b[2J");
                _ = io::stdout().write(b"\x1b[H");
                return false;
            } else if key == ctrl_key!(b'h') {
                // backspace
                editor_del_char(conf);
            } else if key == ctrl_key!(b'l') { // escape
                 // pass
            } else if key == ctrl_key!(b's') {
                editor_save(conf);
            } else if key == ctrl_key!(b'f') {
                editor_find(conf);
            } else {
                editor_insert_char(conf, key);
            }
        }
        EditorKey::HomeKey => conf.cx = 0,
        EditorKey::EndKey => {
            if conf.cy < conf.numrows {
                conf.cx = conf.row[conf.cy as usize].size as u16;
            }
        }
        EditorKey::Backspace => {
            editor_del_char(conf);
        }
        EditorKey::DelKey => {
            editor_move_cursor(conf, EditorKey::ArrowRight);
            editor_del_char(conf);
        }
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
                    EditorKey::PageUp => editor_move_cursor(conf, EditorKey::ArrowUp),
                    _ => editor_move_cursor(conf, EditorKey::ArrowDown),
                }
            }
        }
        EditorKey::ArrowUp
        | EditorKey::ArrowDown
        | EditorKey::ArrowLeft
        | EditorKey::ArrowRight => editor_move_cursor(conf, c),
        EditorKey::Enter => editor_insert_newline(conf),
        EditorKey::Esc => {}
    }

    conf.quit_times = EDITOR_QUIT_TIMES;

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

    editor_set_status_message(
        &mut conf,
        "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find",
    );

    loop {
        editor_refresh_screen(&mut conf);
        if editor_process_keypress(&mut conf) == false {
            break;
        }
    }
}
