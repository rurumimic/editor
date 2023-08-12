package main

import (
	"fmt"
	"io"
	"os"
	// "os/signal"
	// "syscall"
	// "unicode"

	"github.com/pkg/term/termios"
	"golang.org/x/sys/unix"
)

type ExitCode int8

const (
  SUCCESS ExitCode = iota
  FAILURE
)

var orig_termios unix.Termios

func CtrlKey(k byte) byte {
	return k & 0x1f
}

func Die(s string, err error) error {
	fmt.Println(s)
	return err
}

func DisableRawMode() error {
	err := termios.Tcsetattr(os.Stdin.Fd(), termios.TCSAFLUSH, &orig_termios)
	if err != nil {
		return Die("Tcsetattr", err)
	}
  return nil
}

func EnableRawMode() error {
	err := termios.Tcgetattr(os.Stdin.Fd(), &orig_termios)
	if err != nil {
		return Die("Tcgetattr", err)
	}

	var raw unix.Termios = orig_termios

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

func EditorReadKey() (byte, error) {
    var c []byte = make([]byte, 1)

    for {
      _, err := os.Stdin.Read(c)
      if err != nil && err != io.EOF {
        return 0, Die("ReadKey", err)
      }
      return c[0], nil
    }	
}

func EditorRefreshScreen() {
  os.Stdout.WriteString("\x1b[2J")
  os.Stdout.WriteString("\x1b[H")
}

func EditorProcessKeypress() ExitCode {
  c, err := EditorReadKey()
  if err != nil {
    return FAILURE
  }

  if c == CtrlKey('q') {
    os.Stdout.WriteString("\x1b[2J")
    os.Stdout.WriteString("\x1b[H")
    return FAILURE
  }

  return SUCCESS
}

func main() {
	EnableRawMode()
  defer DisableRawMode()

	for {
	  EditorRefreshScreen()
    if EditorProcessKeypress() == FAILURE {
      break
    }

		// if unicode.IsControl(rune(c[0])) {
		// 	fmt.Printf("%d\r\n", c[0])
		// } else {
		// 	fmt.Printf("%d ('%c')\r\n", c[0], c[0])
		// }
	}

}
