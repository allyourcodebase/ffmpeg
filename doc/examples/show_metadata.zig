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

    //AVFormatContext *fmt_ctx = NULL;
    //const AVDictionaryEntry *tag = NULL;
    //int ret;

    //if ((ret = avformat_open_input(&fmt_ctx, argv[1], NULL, NULL)))
    //    return ret;

    //if ((ret = avformat_find_stream_info(fmt_ctx, NULL)) < 0) {
    //    av_log(NULL, AV_LOG_ERROR, "Cannot find stream information\n");
    //    return ret;
    //}

    //while ((tag = av_dict_iterate(fmt_ctx->metadata, tag)))
    //    printf("%s=%s\n", tag->key, tag->value);
}
