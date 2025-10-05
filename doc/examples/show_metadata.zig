//! libavformat metadata extraction API usage example
//!
//! Show metadata from an input file.

const std = @import("std");
const av = @import("av");

pub fn main() !void {
    const gpa = std.heap.raw_c_allocator;

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const args = try std.process.argsAlloc(arena);
    if (args.len != 2) {
        std.debug.print(
            \\usage: {s} <input_file>
            \\example program to demonstrate the use of the libavformat metadata API.
            \\
        , .{args[0]});
        std.process.exit(1);
    }

    const fc = try av.FormatContext.open_input(args[1], null, null, null);
    defer fc.close_input();

    try fc.find_stream_info(null);

    var it: ?*const av.Dictionary.Entry = null;
    while (fc.metadata.iterate(it)) |tag| : (it = tag) {
        var w = std.fs.File.stdout().writer(&.{});
        const stdout = &w.interface;
        try stdout.print("{s}={s}\n", .{ tag.key, tag.value });
    }
}
