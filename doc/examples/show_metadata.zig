//! libavformat metadata extraction API usage example
//!
//! Show metadata from an input file.

const std = @import("std");
const Io = std.Io;

const av = @import("av");

pub fn main(init: std.process.Init) !void {
    const arena = init.arena.allocator();

    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 2) {
        std.debug.print(
            \\usage: {s} <input_file>
            \\example program to demonstrate the use of the libavformat metadata API.
            \\
        , .{args[0]});
        std.process.exit(1);
    }

    const io = init.io;

    const fc = try av.FormatContext.open_input(args[1], null, null, null);
    defer fc.close_input();

    try fc.find_stream_info(null);

    var stdout_buffer: [100]u8 = undefined;
    var stdout_writer = Io.File.stdout().writerStreaming(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    var it: ?*const av.Dictionary.Entry = null;
    while (fc.metadata.iterate(it)) |tag| : (it = tag) {
        try stdout.print("{s}={s}\n", .{ tag.key, tag.value });
    }

    try stdout.flush();
}
