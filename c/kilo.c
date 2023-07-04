/*** includes ***/

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <termio.h>
#include <unistd.h>

/*** data ***/

struct termios orig_termios;

/*** terminal ***/

void die(const char *s) {
    perror(s);
    exit(1);
}

void disableRawMode() {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios) == -1)
        die("tcsetattr");
}

void enableRawMode() {
    // read the current terminal's attributes
    if (tcgetattr(STDIN_FILENO, &orig_termios) == -1)
        die("tcgetattr");
    atexit(disableRawMode); // register disableRawMode to be called
                            // automatically when the program exits

    struct termios raw = orig_termios;

    // local mode flags: miscellaneous flags. dumping ground for other state.
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    // disable ECHO: don't print the input characters
    // disable ICANON: read input byte-by-byte, instead of line-by-line
    // disable IEXTEN: disable Ctrl-V and Ctrl-O signals
    // disable ISIG: disable Ctrl-C and Ctrl-Z signals
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    // disable BRKINT: disable Ctrl-C signal (break condition will cause SIGINT)
    // disable ICRNL: disable Ctrl-M signal. Ctrl-M is carriage return
    // disable INPCK: disable parity checking
    // disable ISTRIP: disable stripping the 8th bit of each input byte
    // disable IXON: disable Ctrl-S and Ctrl-Q signals
    raw.c_oflag &= ~(OPOST);
    // disable OPOST: disable output processing. \n is not translated to \r\n
    raw.c_cflag |= (CS8); // control mode flags
    // enable CS8: set character size to 8 bits per byte
    raw.c_cc[VMIN] = 0; // minimum number of bytes of input needed before read()
    raw.c_cc[VTIME] = 1; // maximum amount of time to wait before read() returns
    // 1 = 100ms timeout

    // TCSAFLUSH:
    // wait until all pending output has been written to the terminal
    // and discards any input that hasn't been read
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
        die("tcsetattr"); // write the terminal's attributes
}

/*** init ***/

int main() {
    enableRawMode();

    while (1) {
        char c = '\0';
        if (read(STDIN_FILENO, &c, 1) == -1 && errno != EAGAIN)
            die("read");

        if (iscntrl(c)) {        // is control character? (ASCII 0-31, 127)
            printf("%d\r\n", c); // print the ASCII code
        } else {                 // is printable character? (ASCII 32-126)
            printf("%d ('%c')\r\n", c, c);
        }

        if (c == 'q')
            break;
    };
    return 0;
}
