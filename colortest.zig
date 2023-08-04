const std = @import("std");
pub fn main() anyerror!void {
    const stdout_writer = std.io.getStdOut().writer();
    // Print the first 16 colors - these vary by terminal configuration
    try stdout_writer.print("\n", .{});
    for (0..16) |i| {
        try stdout_writer.print("\x1b[48;5;{d}m  ", .{i});
    }
    try stdout_writer.print("\x1b[0m\n\n", .{});

    // Print the 6 sides of the color cube - these are more standardized,
    //  but the order is a bit odd, thus the need for this trickery
    {
        // all because Zig doesn't have C-style for loops, this needs to be in its own scope
        var i: u8 = 16;
        while (i < 52) : (i += 6) {
            for (0..6) |ii| {
                try stdout_writer.print("\x1b[48;5;{d}m  ", .{i + ii});
            }
            try stdout_writer.print("\x1b[0m  ", .{});
            for (36..42) |ii| {
                try stdout_writer.print("\x1b[48;5;{d}m  ", .{i + ii});
            }
            try stdout_writer.print("\x1b[0m  ", .{});
            for (72..78) |ii| {
                try stdout_writer.print("\x1b[48;5;{d}m  ", .{i + ii});
            }
            try stdout_writer.print("\x1b[0m\n", .{});
        }
        try stdout_writer.print("\n", .{});
        i = 124;
        while (i < 160) : (i += 6) {
            for (0..6) |ii| {
                try stdout_writer.print("\x1b[48;5;{d}m  ", .{i + ii});
            }
            try stdout_writer.print("\x1b[0m  ", .{});
            for (36..42) |ii| {
                try stdout_writer.print("\x1b[48;5;{d}m  ", .{i + ii});
            }
            try stdout_writer.print("\x1b[0m  ", .{});
            for (72..78) |ii| {
                try stdout_writer.print("\x1b[48;5;{d}m  ", .{i + ii});
            }
            try stdout_writer.print("\x1b[0m\n", .{});
        }
        try stdout_writer.print("\n", .{});
    }
    // Finally, the 24 grays
    for (232..256) |i| {
        try stdout_writer.print("\x1b[48;5;{d}m  ", .{i});
    }
    try stdout_writer.print("\x1b[0m\n\n", .{});
}
