// SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const stdout_writer = std.io.getStdOut().writer();

fn colorCell(n: u8) !void {
    try stdout_writer.print("\x1b[48;5;{d}m  ", .{n});
}

fn cubeRowPart(n: u8) !void {
    for (n..(n + 6)) |i| {
        // Zig for(range) syntax really should allow int types other than usize
        try colorCell(@intCast(i));
    }
}

fn cubeRow(n: u8) !void {
    try cubeRowPart(n);
    try stdout_writer.print("\x1b[0m  ", .{});
    try cubeRowPart(n + 36);
    try stdout_writer.print("\x1b[0m  ", .{});
    try cubeRowPart(n + 72);
    try stdout_writer.print("\x1b[0m\n", .{});
}

pub fn main() !void {
    // Print the first 16 colors - these vary by terminal configuration
    try stdout_writer.print("\n", .{});
    for (0..16) |i| {
        try colorCell(@intCast(i));
    }
    try stdout_writer.print("\x1b[0m\n\n", .{});

    // Print the 6 sides of the color cube - these are more standardized,
    //  but the order is a bit odd, thus the need for this trickery
    {
        // because Zig doesn't have C-style for loops,
        // this should be in its own scope
        var i: u8 = 16;
        while (i < 52) : (i += 6) {
            try cubeRow(i);
        }
        try stdout_writer.print("\n", .{});
        i = 124;
        while (i < 160) : (i += 6) {
            try cubeRow(i);
        }
        try stdout_writer.print("\n", .{});
    }
    // Finally, the 24 grays
    for (232..256) |i| {
        try colorCell(@intCast(i));
    }
    try stdout_writer.print("\x1b[0m\n\n", .{});
}
