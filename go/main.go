package main

import (
	"fmt"
	"io"
	"os"
	"os/signal"
	"syscall"
	"unicode"

	"github.com/pkg/term/termios"
	"golang.org/x/sys/unix"
)

var orig_termios unix.Termios

func CtrlKey(k byte) byte {
	return k & 0x1f
}

func Die(s string) {
	fmt.Println(s)
	os.Exit(1)
}

func DisableRawMode() {
	err := termios.Tcsetattr(os.Stdin.Fd(), termios.TCSAFLUSH, &orig_termios)
	if err != nil {
		Die("Tcsetattr")
	}
}

func EnableRawMode() {
	err := termios.Tcgetattr(os.Stdin.Fd(), &orig_termios)
	if err != nil {
		Die("Tcgetattr")
	}

	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-c
		DisableRawMode()
		os.Exit(1)
	}()

	var raw unix.Termios = orig_termios

	raw.Lflag &^= (unix.ECHO | unix.ICANON | unix.ISIG | unix.IEXTEN)
	raw.Iflag &^= (unix.BRKINT | unix.ICRNL | unix.INPCK | unix.ISTRIP | unix.IXON)
	raw.Oflag &^= unix.OPOST
	raw.Cflag |= (unix.CS8)
	raw.Cc[unix.VMIN] = 0
	raw.Cc[unix.VTIME] = 1

	err = termios.Tcsetattr(os.Stdin.Fd(), termios.TCSAFLUSH, &raw)
	if err != nil {
		Die("Tcsetattr")
	}
}

func main() {
	EnableRawMode()

	for {
		var c []byte = make([]byte, 1)

		_, err := os.Stdin.Read(c)
		if err != nil && err != io.EOF {
			Die("Read")
		}

		if unicode.IsControl(rune(c[0])) {
			fmt.Printf("%d\r\n", c[0])
		} else {
			fmt.Printf("%d ('%c')\r\n", c[0], c[0])
		}

		if c[0] == CtrlKey('q') {
			break
		}
	}

}
