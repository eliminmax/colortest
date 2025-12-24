// SPDX-FileCopyrightText: 2023 - 2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Writer = std.Io.Writer;

fn colorCell(n: u8, writer: *Writer) !void {
    try writer.print("\x1b[48;5;{d}m  ", .{n});
}

fn cubeRowPart(n: u8, writer: *Writer) !void {
    for (n..(n + 6)) |i| {
        // Zig for(range) syntax really should allow int types other than usize
        try colorCell(@intCast(i), writer);
    }
}

fn cubeRow(n: u8, writer: *Writer) !void {
    try cubeRowPart(n, writer);
    try writer.writeAll("\x1b[0m  ");
    try cubeRowPart(n + 36, writer);
    try writer.writeAll("\x1b[0m  ");
    try cubeRowPart(n + 72, writer);
    try writer.writeAll("\x1b[0m\n");
}

pub fn main() !void {
    // Print the first 16 colors - these vary by terminal configuration
    var buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&buffer);
    var writer = &stdout_writer.interface;

    try writer.writeAll("\n");
    for (0..16) |i| {
        try colorCell(@intCast(i), writer);
    }
    try writer.writeAll("\x1b[0m\n\n");

    // Print the 6 sides of the color cube - these are more standardized,
    //  but the order is a bit odd, thus the need for this trickery
    {
        // because Zig doesn't have C-style for loops,
        // this should be in its own scope
        var i: u8 = 16;
        while (i < 52) : (i += 6) {
            try cubeRow(i, writer);
        }
        try writer.writeAll("\n");
        i = 124;
        while (i < 160) : (i += 6) {
            try cubeRow(i, writer);
        }
        try writer.writeAll("\n");
    }
    // Finally, the 24 grays
    for (232..256) |i| {
        try colorCell(@intCast(i), writer);
    }
    try writer.writeAll("\x1b[0m\n\n");
    try writer.flush();
}
