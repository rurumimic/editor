const std = @import("std");
const stdlib = @cImport({@cInclude("stdlib.h");});
const os = std.os;

var orig_termios: os.termios = undefined;

fn die(s: []const u8) noreturn {
    std.io.getStdErr().writeAll(s) catch unreachable;
    os.exit(1);
}

fn disableRawMode() callconv(.C) void {
    os.tcsetattr(os.STDIN_FILENO, os.TCSA.FLUSH, orig_termios) catch {
        die("tcsetattr");
    };
}

fn enableRawMode() void {
    orig_termios = os.tcgetattr(os.STDIN_FILENO) catch {
        die("tcgetattr");
    };
    const result = stdlib.atexit(disableRawMode);
    if (result != 0) {
        std.debug.print("atexit failed\r\n", .{});
        os.exit(1);
    }

    var raw = orig_termios;
    raw.lflag &= ~(os.linux.ECHO | os.linux.ICANON | os.linux.IEXTEN | os.linux.ISIG);
    raw.iflag &= ~(os.linux.BRKINT | os.linux.ICRNL | os.linux.INPCK | os.linux.ISTRIP | os.linux.IXON);
    raw.oflag &= ~os.linux.OPOST;
    raw.cflag |= os.linux.CS8;
    raw.cc[6] = 0;
    raw.cc[5] = 1;

    os.tcsetattr(os.STDIN_FILENO, os.TCSA.FLUSH, raw) catch {
        die("tcsetattr");
    };
}

pub fn main() !void {
    enableRawMode();

    while (true) {
        var c: [1]u8 = [_]u8{0};
        if (try std.io.getStdIn().read(&c) == -1 and os.errno != os.EAGAIN) {
            die("read");
        }

        if (std.ascii.isControl(c[0])) {
            std.debug.print("{d}\r\n", .{c[0]});
        } else {
            std.debug.print("{d} ('{c}')\r\n", .{c[0], c[0]});
        }

        if (c[0] == 'q') {
            break;
        }
    }
}
