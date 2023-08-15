package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"

	"github.com/pkg/term/termios"
	"golang.org/x/sys/unix"
)

const EDITOR_VERSION = "0.0.1"

type ExitCode int8

const (
	SUCCESS ExitCode = iota
	FAILURE
)

type EditorConfig struct {
	cx           int
	cy           int
	screenrows   int
	screencols   int
	orig_termios unix.Termios
}

var E EditorConfig

func CtrlKey(k byte) byte {
	return k & 0x1f
}

const (
	ArrowLeft = 1000 + iota
	ArrowRight
	ArrowUp
	ArrowDown
	DelKey
	HomeKey
	EndKey
	PageUp
	PageDown
)

func Die(s string, err error) error {
	os.Stdout.WriteString("\x1b[2J")
	os.Stdout.WriteString("\x1b[H")
	fmt.Println(s)
	return err
}

func DisableRawMode() error {
	err := termios.Tcsetattr(os.Stdin.Fd(), termios.TCSAFLUSH, &E.orig_termios)
	if err != nil {
		return Die("Tcsetattr", err)
	}
	return nil
}

func EnableRawMode() error {
	err := termios.Tcgetattr(os.Stdin.Fd(), &E.orig_termios)
	if err != nil {
		return Die("Tcgetattr", err)
	}

	var raw unix.Termios = E.orig_termios

	raw.Lflag &^= (unix.ECHO | unix.ICANON | unix.ISIG | unix.IEXTEN)
	raw.Iflag &^= (unix.BRKINT | unix.ICRNL | unix.INPCK | unix.ISTRIP | unix.IXON)
	raw.Oflag &^= unix.OPOST
	raw.Cflag |= (unix.CS8)
	raw.Cc[unix.VMIN] = 0
	raw.Cc[unix.VTIME] = 1

	err = termios.Tcsetattr(os.Stdin.Fd(), termios.TCSAFLUSH, &raw)
	if err != nil {
		return Die("Tcsetattr", err)
	}
	return nil
}

func EditorReadKey() (int, error) {
	var c []byte = make([]byte, 1)

	for {
		_, err := os.Stdin.Read(c)
		if err != nil && err != io.EOF {
			return 0, Die("ReadKey", err)
		}
		break
	}

	if c[0] == '\x1b' {
		var seq0 []byte = make([]byte, 1)
		var seq1 []byte = make([]byte, 1)
		var seq2 []byte = make([]byte, 1)
		_, err := os.Stdin.Read(seq0)
		if err != nil {
			return '\x1b', nil
		}
		_, err = os.Stdin.Read(seq1)
		if err != nil {
			return '\x1b', nil
		}

		if seq0[0] == '[' {
			if seq1[0] >= '0' && seq1[0] <= '9' {
				_, err = os.Stdin.Read(seq2)
				if err != nil {
					return '\x1b', nil
				}
				if seq2[0] == '~' {
					switch seq1[0] {
					case '1':
						return HomeKey, nil
					case '3':
						return DelKey, nil
					case '4':
						return EndKey, nil
					case '5':
						return PageUp, nil
					case '6':
						return PageDown, nil
					case '7':
						return HomeKey, nil
					case '8':
						return EndKey, nil
					}
				}
			} else {
				switch seq1[0] {
				case 'A':
					return ArrowUp, nil
				case 'B':
					return ArrowDown, nil
				case 'C':
					return ArrowRight, nil
				case 'D':
					return ArrowLeft, nil
				case 'H':
					return HomeKey, nil
				case 'F':
					return EndKey, nil
				}
			}
		} else if seq0[0] == 'O' {
			switch seq1[0] {
			case 'H':
				return HomeKey, nil
			case 'F':
				return EndKey, nil
			}
		}
		return '\x1b', nil
	}

	return int(c[0]), nil
}

func GetCursorPosition() (int, int, error) {
	_, err := os.Stdout.WriteString("\x1b[6n")
	if err != nil {
		return -1, -1, nil
	}

	r := bufio.NewReader(os.Stdin)
	s, err := r.ReadString('R')
	if err != nil {
		return -1, -1, err
	}

	row := -1
	col := -1

	_, err = fmt.Sscanf(s, "\x1b[%d;%dR", &row, &col)
	if err != nil {
		return -1, -1, err
	}

	return row, col, nil
}

func GetWindowSize() (int, int, error) {
	ws, err := unix.IoctlGetWinsize(int(os.Stdout.Fd()), unix.TIOCGWINSZ)
	if err != nil || ws.Col == 0 {
		_, err = os.Stdout.WriteString("\x1b[999C\x1b[999B")
		if err != nil {
			return -1, -1, err
		}
		return GetCursorPosition()
	}

	return int(ws.Row), int(ws.Col), nil
}

type Abuf struct {
	b bytes.Buffer
}

func (abuf *Abuf) append(s string) {
	abuf.b.WriteString(s)
}

func EditorDrawRows(abuf *Abuf) {
	for y := 0; y < E.screenrows; y++ {
		if y == E.screenrows/3 {
			welcome := fmt.Sprintf("The Editor -- version %s", EDITOR_VERSION)
			welcomelen := len(welcome)
			if welcomelen > E.screencols {
				welcomelen = E.screencols
			}
			padding := (E.screencols - welcomelen) / 2
			if padding > 0 {
				abuf.append("~")
				padding--
			}
			for ; padding > 0; padding-- {
				abuf.append(" ")
			}
			abuf.append(welcome[:welcomelen])
		} else {
			abuf.append("~")
		}

		abuf.append("\x1b[K")
		if y < E.screenrows-1 {
			abuf.append("\r\n")
		}
	}
}

func EditorRefreshScreen() {
	var abuf Abuf

	abuf.append("\x1b[?25l")
	abuf.append("\x1b[H")

	EditorDrawRows(&abuf)

	cursor := fmt.Sprintf("\x1b[%d;%dH", E.cy+1, E.cx+1)
	abuf.append(cursor)

	abuf.append("\x1b[?25h")

	os.Stdout.WriteString(abuf.b.String())
}

func EditorMoveCursor(key int) {
	switch key {
	case ArrowLeft:
		if E.cx != 0 {
			E.cx--
		}
	case ArrowRight:
		if E.cx != E.screencols-1 {
			E.cx++
		}
	case ArrowUp:
		if E.cy != 0 {
			E.cy--
		}
	case ArrowDown:
		if E.cy != E.screenrows-1 {
			E.cy++
		}
	}
}

func EditorProcessKeypress() ExitCode {
	c, err := EditorReadKey()
	if err != nil {
		return FAILURE
	}

	if byte(c) == CtrlKey('q') {
		os.Stdout.WriteString("\x1b[2J")
		os.Stdout.WriteString("\x1b[H")
		return FAILURE
	}

	switch c {
	case HomeKey:
		E.cx = 0
	case EndKey:
		E.cx = E.screencols - 1
	case PageUp, PageDown:
		for times := E.screenrows; times > 0; times-- {
			switch c {
			case PageUp:
				EditorMoveCursor(ArrowUp)
			case PageDown:
				EditorMoveCursor(ArrowDown)
			}
		}
	case ArrowUp, ArrowDown, ArrowLeft, ArrowRight:
		EditorMoveCursor(c)
	}

	return SUCCESS
}

func InitEditor() error {
	row, col, err := GetWindowSize()
	if err != nil {
		return Die("GetWindowSize", err)
	}

	E.cx = 0
	E.cy = 0
	E.screenrows = row
	E.screencols = col
	return nil
}

func main() {
	EnableRawMode()
	defer DisableRawMode()

	if InitEditor() != nil {
		return
	}

	for {
		EditorRefreshScreen()
		if EditorProcessKeypress() == FAILURE {
			break
		}
	}

}
