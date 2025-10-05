const std = @import("std");
const assert = std.debug.assert;

/// Prefer `FormatContext.alloc`.
pub extern fn avformat_alloc_context() ?*FormatContext;
/// Prefer `FormatContext.free`.
pub extern fn avformat_free_context(?*FormatContext) void;
/// Prefer `FormatContext.open_input`.
pub extern fn avformat_open_input(ps: *?*FormatContext, url: [*:0]const u8, fmt: ?*const InputFormat, options: ?*Dictionary.Mutable) c_int;
/// Prefer `FormatContext.close_input`.
pub extern fn avformat_close_input(s: *?*FormatContext) void;
/// Prefer `FormatContext.find_stream_info`.
pub extern fn avformat_find_stream_info(ic: *FormatContext, options: ?[*]Dictionary.Mutable) c_int;
/// Prefer `FormatContext.find_best_stream`.
pub extern fn av_find_best_stream(
    ic: *FormatContext,
    media_type: MediaType,
    wanted_stream_nb: c_int,
    related_stream: c_int,
    decoder_ret: ?*?*const Codec,
    flags: c_int,
) c_int;
/// Prefer `FormatContext.read_frame`.
pub extern fn av_read_frame(s: *FormatContext, pkt: *Packet) c_int;
/// Prefer `FormatContext.seek_frame`.
pub extern fn av_seek_frame(s: *FormatContext, stream_index: c_int, timestamp: i64, flags: c_int) c_int;
/// Prefer `FormatContext.flush`.
pub extern fn avformat_flush(s: *FormatContext) c_int;
/// Prefer `FormatContext.dump`
pub extern fn av_dump_format(ic: *FormatContext, index: c_uint, url: ?[*:0]const u8, is_output: enum(c_int) { input, output }) void;

/// Prefer `IOContext.alloc`.
pub extern fn avio_alloc_context(
    buffer: [*c]u8,
    buffer_size: c_int,
    write_flag: IOContext.WriteFlag,
    @"opaque": ?*anyopaque,
    read_packet: ?*const fn (?*anyopaque, [*:0]u8, c_int) callconv(.c) c_int,
    write_packet: ?*const fn (?*anyopaque, [*:0]u8, c_int) callconv(.c) c_int,
    seek: ?*const fn (?*anyopaque, i64, SEEK) callconv(.c) i64,
) [*c]IOContext;
/// Prefer `IOContext.free`.
pub extern fn avio_context_free(s: *?*IOContext) void;
/// Prefer `IOContext.close`.
pub extern fn avio_close(s: ?*IOContext) c_int;

/// Prefer `LOG.set`.
pub extern fn av_log_set_level(level: LOG) void;
/// Prefer `malloc`.
pub extern fn av_malloc(size: usize) ?[*]u8;
/// Prefer `free`.
pub extern fn av_free(ptr: ?*anyopaque) void;
/// Prefer `Dictionary.Const.get` or `Dictionary.Mutable.get`
pub extern fn av_dict_get(m: Dictionary.Const, key: [*:0]const u8, prev: ?*const Dictionary.Entry, flags: Dictionary.Flags) ?*const Dictionary.Entry;
/// Prefer `Dictionary.Const.iterate` or `Dictionary.Mutable.iterate`.
pub extern fn av_dict_iterate(m: Dictionary.Const, prev: ?*const Dictionary.Entry) ?*const Dictionary.Entry;
/// Prefer `Dictionary.Const.count` or `Dictionary.Mutable.count`.
pub extern fn av_dict_count(m: Dictionary.Const) c_int;
/// Prefer `Dictionary.Mutable.set`.
pub extern fn av_dict_set(pm: *Dictionary.Mutable, key: [*:0]const u8, value: ?[*:0]const u8, flags: Dictionary.Flags) c_int;
/// Prefer `Dictionary.Mutable.set_int`.
pub extern fn av_dict_set_int(pm: *Dictionary.Mutable, key: [*:0]const u8, value: i64, flags: Dictionary.Flags) c_int;
/// Prefer `Dictionary.Mutable.copy`.
pub extern fn av_dict_copy(dst: *Dictionary.Mutable, src: Dictionary.Const, flags: Dictionary.Flags) void;
/// Prefer `Dictionary.Const.free` or `Dictionary.Mutable.free`.
pub extern fn av_dict_free(pm: *Dictionary.Const) void;

/// Prefer `FilterContext.opt_set`.
pub extern fn av_opt_set(obj: *anyopaque, name: [*:0]const u8, val: [*:0]const u8, search_flags: OPT_SEARCH) c_int;
/// Prefer `FilterContext.opt_set_int`.
pub extern fn av_opt_set_int(obj: *anyopaque, name: [*:0]const u8, val: i64, search_flags: OPT_SEARCH) c_int;
/// Prefer `FilterContext.opt_set_double`.
pub extern fn av_opt_set_double(obj: *anyopaque, name: [*:0]const u8, val: f64, search_flags: OPT_SEARCH) c_int;
/// Prefer `FilterContext.opt_set_q`.
pub extern fn av_opt_set_q(obj: *anyopaque, name: [*:0]const u8, val: Rational, search_flags: OPT_SEARCH) c_int;
/// Prefer `FilterContext.opt_get_double`.
pub extern fn av_opt_get_double(obj: *anyopaque, name: [*:0]const u8, search_flags: OPT_SEARCH, out_val: *f64) c_int;
/// Prefer `FilterContext.init_str`.
pub extern fn avfilter_init_str(ctx: *FilterContext, args: ?[*:0]const u8) c_int;
/// Prefer `FilterContext.link`.
pub extern fn avfilter_link(src: *FilterContext, srcpad: c_uint, dst: *FilterContext, dstpad: c_uint) c_int;
/// Prefer `FilterContext.buffersrc_write_frame`.
pub extern fn av_buffersrc_write_frame(ctx: *FilterContext, frame: *const Frame) c_int;
/// Prefer `FilterContext.buffersrc_add_frame`.
pub extern fn av_buffersrc_add_frame(ctx: *FilterContext, frame: ?*Frame) c_int;
/// Prefer `FilterContext.buffersink_get_frame_flags`.
pub extern fn av_buffersink_get_frame_flags(ctx: *FilterContext, frame: *Frame, flags: BUFFERSINK_FLAG) c_int;
/// Prefer `FilterContext.buffersink_get_samples`.
pub extern fn av_buffersink_get_samples(ctx: *FilterContext, frame: *Frame, nb_samples: c_int) c_int;
/// Prefer `FilterContext.buffersink_set_frame_size`.
pub extern fn av_buffersink_set_frame_size(ctx: *FilterContext, frame_size: c_uint) void;

/// Prefer `CodecContext.alloc`.
pub extern fn avcodec_alloc_context3(codec: *const Codec) ?*Codec.Context;
/// Prefer `CodecContext.free`.
pub extern fn avcodec_free_context(avctx: *?*Codec.Context) void;
/// Prefer `CodecContext.parameters_to_context`.
pub extern fn avcodec_parameters_to_context(codec: *Codec.Context, par: *const Codec.Parameters) c_int;
/// Prefer `CodecContext.avcodec_open`.
pub extern fn avcodec_open2(avctx: *Codec.Context, codec: *const Codec, options: ?*Dictionary.Mutable) c_int;
/// Prefer `CodecContext.send_packet`.
pub extern fn avcodec_send_packet(avctx: *Codec.Context, avpkt: ?*const Packet) c_int;
/// Prefer `CodecContext.receive_frame`.
pub extern fn avcodec_receive_frame(avctx: *Codec.Context, frame: *Frame) c_int;
/// Prefer `CodecContext.flush_buffers`.
pub extern fn avcodec_flush_buffers(avctx: *Codec.Context) void;

/// Prefer `Codec.iterate`
pub extern fn av_codec_iterate(@"opaque": *?*Codec.Iterator) ?*const Codec;
/// Prefer `Codec.find_decoder`
pub extern fn avcodec_find_decoder(id: Codec.ID) ?*const Codec;
/// Prefer `Codec.find_decoder_by_name`
pub extern fn avcodec_find_decoder_by_name(name: [*:0]const u8) ?*const Codec;
/// Prefer `Codec.find_encoder`
pub extern fn avcodec_find_encoder(id: Codec.ID) ?*const Codec;
/// Prefer `Codec.find_encoder_by_name`
pub extern fn avcodec_find_encoder_by_name(name: [*:0]const u8) ?*const Codec;
/// Prefer `Codec.is_encoder`
pub extern fn av_codec_is_encoder(codec: *const Codec) c_int;
/// Prefer `Codec.is_decoder`
pub extern fn av_codec_is_decoder(codec: *const Codec) c_int;
/// Prefer `Codec.get_profile_name`
pub extern fn av_get_profile_name(codec: *const Codec, profile: c_int) ?[*:0]const u8;

/// Prefer `Packet.alloc`.
pub extern fn av_packet_alloc() ?*Packet;
/// Prefer `Packet.free`.
pub extern fn av_packet_free(pkt: *?*Packet) void;
/// Prefer `Packet.ref`.
pub extern fn av_packet_ref(dst: *Packet, src: *const Packet) c_int;
/// Prefer `Packet.unref`.
pub extern fn av_packet_unref(pkt: *Packet) void;

/// Prefer `Frame.alloc`.
pub extern fn av_frame_alloc() ?*Frame;
/// Prefer `Frame.free`.
pub extern fn av_frame_free(frame: *?*Frame) void;
/// Prefer `Frame.ref`.
pub extern fn av_frame_ref(dst: *Frame, src: *const Frame) c_int;
/// Prefer `Frame.unref`.
pub extern fn av_frame_unref(frame: *Frame) void;

/// Prefer `FilterGraph.alloc`.
pub extern fn avfilter_graph_alloc() ?*FilterGraph;
/// Prefer `FilterGraph.free`.
pub extern fn avfilter_graph_free(graph: *?*FilterGraph) void;
/// Prefer `FilterGraph.alloc_filter`.
pub extern fn avfilter_graph_alloc_filter(graph: *FilterGraph, filter: *const Filter, name: ?[*:0]const u8) ?*FilterContext;
/// Prefer `FilterGraph.config`.
pub extern fn avfilter_graph_config(graphctx: *FilterGraph, log_ctx: ?*anyopaque) c_int;

/// Prefer `Filter.get_by_name`.
pub extern fn avfilter_get_by_name(name: [*:0]const u8) ?*const Filter;

/// Prefer `ChannelLayout.compare`.
pub extern fn av_channel_layout_compare(a: *const ChannelLayout, b: *const ChannelLayout) c_int;
/// Prefer `ChannelLayout.uninit`.
pub extern fn av_channel_layout_uninit(channel_layout: *ChannelLayout) void;
/// Prefer `ChannelLayout.describe`.
pub extern fn av_channel_layout_describe(channel_layout: *const ChannelLayout, buf: [*]u8, buf_size: usize) c_int;
/// Prefer `ChannelLayout.from_mask`.
pub extern fn av_channel_layout_from_mask(channel_layout: *ChannelLayout, mask: u64) c_int;

/// Prefer `SampleFormat.get_name`.
pub extern fn av_get_sample_fmt_name(sample_fmt: SampleFormat) ?[*:0]const u8;
/// Prefer `SampleFormat.get_bytes_per_sample`.
pub extern fn av_get_bytes_per_sample(sample_fmt: SampleFormat) c_int;
/// Prefer `SampleFormat.is_planar`.
pub extern fn av_sample_fmt_is_planar(sample_fmt: SampleFormat) c_int;

/// Prefer `RDFTContext.init`.
pub extern fn av_rdft_init(nbits: c_int, trans: RDFTransformType) ?*RDFTContext;
/// Prefer `RDFTContext.calc`.
pub extern fn av_rdft_calc(s: *RDFTContext, data: [*]FFTSample) void;
/// Prefer `RDFTContext.end`.
pub extern fn av_rdft_end(s: *RDFTContext) void;

/// Prefer `TXContext.init`.
pub extern fn av_tx_init(
    ctx: *?*TXContext,
    tx: *?*const tx_fn,
    @"type": TXType,
    inv: c_int,
    len: c_int,
    scale: ?*const anyopaque,
    flags: TXFlags,
) c_int;
/// Prefer `TXContext.uninit`.
pub extern fn av_tx_uninit(ctx: *?*TXContext) void;

/// Prefer `sws.Context.alloc`.
pub extern fn sws_alloc_context() ?*sws.Context;
/// Prefer `sws.Context.init`.
pub extern fn sws_init_context(sws_context: *sws.Context, srcFilter: ?*sws.Filter, dstFilter: ?*sws.Filter) c_int;
/// Prefer `sws.Context.free`.
pub extern fn sws_freeContext(swsContext: ?*sws.Context) void;
/// Prefer `sws.Context.get`.
pub extern fn sws_getContext(srcW: c_int, srcH: c_int, srcFormat: PixelFormat, dstW: c_int, dstH: c_int, dstFormat: PixelFormat, flags: sws.Flags, srcFilter: ?*sws.Filter, dstFilter: ?*sws.Filter, ?[*]const f64) ?*sws.Context;
/// Prefer `sws.Context.scale`.
pub extern fn sws_scale(c: *sws.Context, srcSlice: [*]const [*]const u8, srcStride: [*]const c_int, srcSliceY: c_int, srcSliceH: c_int, dst: [*]const [*]u8, dstStride: [*]const c_int) c_int;
/// Prefer `sws.Context.scale_frame`.
pub extern fn sws_scale_frame(c: *sws.Context, dst: *Frame, src: *const Frame) c_int;

/// Function pointer to a function to perform the transform.
///
/// Using a different context than the one allocated during `av_tx_init` is not
/// allowed.
///
/// The out and in arrays must be aligned to the maximum required by the CPU
/// architecture unless the `TXFlags.UNALIGNED` flag was set in `av_tx_init`.
///
/// The stride must follow the constraints the transform type has specified.
pub const tx_fn = fn (
    s: *TXContext,
    output_array: ?*anyopaque,
    input_array: ?*anyopaque,
    stride_in_bytes: isize,
) callconv(.c) void;

pub fn malloc(size: usize) error{OutOfMemory}![]u8 {
    const ptr = av_malloc(size) orelse return error.OutOfMemory;
    return ptr[0..size];
}

pub const free = av_free;

/// Undefined timestamp value.
///
/// Usually reported by demuxer that work on containers that do not provide
/// either pts or dts.
pub const NOPTS_VALUE: i64 = @bitCast(@as(u64, 0x8000000000000000));

pub const OPT_SEARCH = packed struct(c_int) {
    CHILDREN: bool = false,
    FAKE_OBJ: bool = false,
    _: u30 = 0,
};

pub const LOG = enum(c_int) {
    QUIET = -8,
    PANIC = 0,
    FATAL = 8,
    ERROR = 16,
    WARNING = 24,
    INFO = 32,
    VERBOSE = 40,
    DEBUG = 48,
    TRACE = 56,

    pub fn set_level(level: LOG) void {
        av_log_set_level(level);
    }
};

fn wrap(averror: c_int) Error!c_uint {
    if (averror >= 0) return @intCast(averror);
    const E = std.posix.E;
    return switch (averror) {
        0 => unreachable, // handled above

        -@as(c_int, @intFromEnum(E.INVAL)) => return error.FFmpegInvalid,
        -@as(c_int, @intFromEnum(E.NOENT)) => return error.FileNotFound,
        -@as(c_int, @intFromEnum(E.NOMEM)) => return error.OutOfMemory,
        -@as(c_int, @intFromEnum(E.PERM)) => return error.PermissionDenied,
        -@as(c_int, @intFromEnum(E.AGAIN)) => return error.WouldBlock,
        -@as(c_int, @intFromEnum(E.RANGE)) => return error.OutOfRange,

        @intFromEnum(ERROR.BSF_NOT_FOUND) => return error.BsfNotFound,
        @intFromEnum(ERROR.BUG) => return error.FFmpegBug,
        @intFromEnum(ERROR.BUG2) => return error.FFmpegBug,
        @intFromEnum(ERROR.BUFFER_TOO_SMALL) => return error.BufferTooSmall,
        @intFromEnum(ERROR.DECODER_NOT_FOUND) => return error.DecoderNotFound,
        @intFromEnum(ERROR.DEMUXER_NOT_FOUND) => return error.DemuxerNotFound,
        @intFromEnum(ERROR.ENCODER_NOT_FOUND) => return error.EncoderNotFound,
        @intFromEnum(ERROR.EOF) => return error.EndOfFile,
        @intFromEnum(ERROR.EXIT) => return error.FFmpegExit,
        @intFromEnum(ERROR.EXTERNAL) => return error.FFmpegDependencyFailure,
        @intFromEnum(ERROR.UNKNOWN) => return error.FFmpegDependencyFailure,
        @intFromEnum(ERROR.FILTER_NOT_FOUND) => return error.FilterNotFound,
        @intFromEnum(ERROR.INVALIDDATA) => return error.InvalidData,
        @intFromEnum(ERROR.MUXER_NOT_FOUND) => return error.MuxerNotFound,
        @intFromEnum(ERROR.OPTION_NOT_FOUND) => return error.OptionNotFound,
        @intFromEnum(ERROR.PATCHWELCOME) => return error.FFmpegUnimplemented,
        @intFromEnum(ERROR.PROTOCOL_NOT_FOUND) => return error.ProtocolNotFound,
        @intFromEnum(ERROR.STREAM_NOT_FOUND) => return error.StreamNotFound,
        @intFromEnum(ERROR.EXPERIMENTAL) => return error.FFmpegExperimentalFeature,
        @intFromEnum(ERROR.INPUT_CHANGED) => unreachable, // not legal to use with wrap()
        @intFromEnum(ERROR.OUTPUT_CHANGED) => unreachable, // not legal to use with wrap()
        @intFromEnum(ERROR.HTTP_BAD_REQUEST) => return error.HttpBadRequest,
        @intFromEnum(ERROR.HTTP_UNAUTHORIZED) => return error.HttpUnauthorized,
        @intFromEnum(ERROR.HTTP_FORBIDDEN) => return error.HttpForbidden,
        @intFromEnum(ERROR.HTTP_NOT_FOUND) => return error.HttpNotFound,
        @intFromEnum(ERROR.HTTP_OTHER_4XX) => return error.HttpOther4xx,
        @intFromEnum(ERROR.HTTP_SERVER_ERROR) => return error.Http5xx,

        else => {
            std.log.debug("unexpected ffmpeg error code: {d}", .{averror});
            return error.Unexpected;
        },
    };
}

pub const Error = error{
    FileNotFound,
    OutOfMemory,
    PermissionDenied,
    OutOfRange,

    /// Bitstream filter not found
    BsfNotFound,
    /// Internal FFmpeg bug
    FFmpegBug,
    /// Usually indicates invalid API usage, which would have been an assertion
    /// rather than an error, but is also returned for input files that failed
    /// to demux or decode.
    FFmpegInvalid,
    BufferTooSmall,
    DecoderNotFound,
    DemuxerNotFound,
    EncoderNotFound,
    /// * The decoder has been flushed, and no new packets can be sent to it
    ///   (also returned if more than 1 flush packet is sent)
    /// * The codec has been fully flushed, and there will be no more output
    ///   frames.
    EndOfFile,
    /// Immediate exit was requested; the called function should not be restarted.
    FFmpegExit,
    /// Generic error in an external library.
    FFmpegDependencyFailure,
    FilterNotFound,
    /// Invalid data found when processing input
    InvalidData,
    MuxerNotFound,
    OptionNotFound,
    /// Not yet implemented in FFmpeg, patches welcome.
    FFmpegUnimplemented,
    ProtocolNotFound,
    StreamNotFound,
    /// Requested feature is flagged experimental. Set strict_std_compliance if you really want to use it.
    FFmpegExperimentalFeature,

    /// * input is not accepted in the current state - user must read output with
    ///   `Codec.Context.receive_frame` (once all output is read, the packet
    ///   should be resent, and the call will not fail with WouldBlock).
    /// * output is not available in this state - user must try to send new input.
    WouldBlock,

    HttpBadRequest,
    HttpUnauthorized,
    HttpForbidden,
    HttpNotFound,
    HttpOther4xx,
    Http5xx,

    /// FFmpeg returned an undocumented error code.
    Unexpected,
};

pub const ERROR = enum(c_int) {
    BSF_NOT_FOUND = TAG(0xF8, 'B', 'S', 'F'),
    BUG = TAG('B', 'U', 'G', '!'),
    BUFFER_TOO_SMALL = TAG('B', 'U', 'F', 'S'),
    DECODER_NOT_FOUND = TAG(0xF8, 'D', 'E', 'C'),
    DEMUXER_NOT_FOUND = TAG(0xF8, 'D', 'E', 'M'),
    ENCODER_NOT_FOUND = TAG(0xF8, 'E', 'N', 'C'),
    EOF = TAG('E', 'O', 'F', ' '),
    EXIT = TAG('E', 'X', 'I', 'T'),
    EXTERNAL = TAG('E', 'X', 'T', ' '),
    FILTER_NOT_FOUND = TAG(0xF8, 'F', 'I', 'L'),
    INVALIDDATA = TAG('I', 'N', 'D', 'A'),
    MUXER_NOT_FOUND = TAG(0xF8, 'M', 'U', 'X'),
    OPTION_NOT_FOUND = TAG(0xF8, 'O', 'P', 'T'),
    PATCHWELCOME = TAG('P', 'A', 'W', 'E'),
    PROTOCOL_NOT_FOUND = TAG(0xF8, 'P', 'R', 'O'),
    STREAM_NOT_FOUND = TAG(0xF8, 'S', 'T', 'R'),
    BUG2 = TAG('B', 'U', 'G', ' '),
    UNKNOWN = TAG('U', 'N', 'K', 'N'),
    EXPERIMENTAL = -@as(i32, @bitCast(@as(u32, 0x2bb2afa8))),
    INPUT_CHANGED = -@as(i32, @bitCast(@as(u32, 0x636e6701))),
    OUTPUT_CHANGED = -@as(i32, @bitCast(@as(u32, 0x636e6702))),
    HTTP_BAD_REQUEST = TAG(0xF8, '4', '0', '0'),
    HTTP_UNAUTHORIZED = TAG(0xF8, '4', '0', '1'),
    HTTP_FORBIDDEN = TAG(0xF8, '4', '0', '3'),
    HTTP_NOT_FOUND = TAG(0xF8, '4', '0', '4'),
    HTTP_OTHER_4XX = TAG(0xF8, '4', 'X', 'X'),
    HTTP_SERVER_ERROR = TAG(0xF8, '5', 'X', 'X'),

    pub fn TAG(a: u8, b: u8, c: u8, d: u8) i32 {
        const aw: u32 = a;
        const bw: u32 = b;
        const cw: u32 = c;
        const dw: u32 = d;
        const signed: i32 = (aw << 0) | (bw << 8) | (cw << 16) | (dw << 24);
        return -signed;
    }
};

/// Format I/O context.
///
/// New fields can be added to the end with minor version bumps.
/// Removal, reordering and changes to existing fields require a major
/// version bump.
/// `@sizeOf(FormatContext)` must not be used outside libav*, use
/// `FormatContext.alloc` to create a `FormatContext`.
///
/// Fields can be accessed through AVOptions (av_opt*),
/// the name string used matches the associated command line parameter name and
/// can be found in libavformat/options_table.h.
/// The AVOption/command line parameter names differ in some cases from the C
/// structure field names for historic reasons or brevity.
pub const FormatContext = extern struct {
    /// A class for logging and @ref avoptions. Set by
    /// avformat_alloc_context(). Exports (de)muxer private options if they
    /// exist.
    av_class: *const Class,

    /// The input container format.
    ///
    /// Demuxing only, set by `open_input`.
    iformat: *const InputFormat,

    /// The output container format.
    ///
    /// Muxing only, must be set by the caller before avformat_write_header().
    oformat: *const OutputFormat,

    /// Format private data. This is an AVOptions-enabled struct
    /// if and only if iformat/oformat.priv_class is not NULL.
    ///
    /// - muxing: set by avformat_write_header()
    /// - demuxing: set by `open_input`
    priv_data: ?*anyopaque,

    /// I/O context.
    ///
    /// - demuxing: either set by the user before `open_input` (then
    ///             the user must close it manually) or set by `open_input`.
    /// - muxing: set by the user before avformat_write_header(). The caller must
    ///           take care of closing / freeing the IO context.
    ///
    /// Do NOT set this field if AVFMT_NOFILE flag is set in
    /// iformat/oformat.flags. In such a case, the (de)muxer will handle
    /// I/O in some other way and this field will be NULL.
    pb: ?*IOContext,

    /// Flags signalling stream properties. A combination of AVFMTCTX_*.
    /// Set by libavformat.
    ctx_flags: c_int,

    /// Number of elements in AVFormatContext.streams.
    ///
    /// Set by avformat_new_stream(), must not be modified by any other code.
    nb_streams: c_uint,
    /// A list of all streams in the file. New streams are created with
    /// avformat_new_stream().
    ///
    /// - demuxing: streams are created by libavformat in `open_input`.
    ///             If AVFMTCTX_NOHEADER is set in ctx_flags, then new streams may also
    ///             appear in av_read_frame().
    /// - muxing: streams are created by the user before avformat_write_header().
    ///
    /// Freed by libavformat in avformat_free_context().
    streams: [*]*Stream,

    /// Number of elements in `stream_groups`.
    ///
    /// Set by avformat_stream_group_create(); must not be modified by any other code.
    nb_stream_groups: c_uint,
    /// A list of all stream groups in the file.
    ///
    /// New groups are created with avformat_stream_group_create(), and filled
    /// with avformat_stream_group_add_stream().
    ///
    /// - demuxing: groups may be created by libavformat in avformat_open_input().
    ///             If AVFMTCTX_NOHEADER is set in ctx_flags, then new groups may also
    ///             appear in av_read_frame().
    /// - muxing: groups may be created by the user before avformat_write_header().
    ///
    /// Freed by libavformat in `free`.
    stream_groups: [*]*StreamGroup,

    /// Number of chapters in `Chapter` array.
    /// When muxing, chapters are normally written in the file header,
    /// so nb_chapters should normally be initialized before write_header
    /// is called. Some muxers (e.g. mov and mkv) can also write chapters
    /// in the trailer.  To write chapters in the trailer, nb_chapters
    /// must be zero when write_header is called and non-zero when
    /// write_trailer is called.
    /// - muxing: set by user
    /// - demuxing: set by libavformat
    nb_chapters: c_uint,
    chapters: [*]*Chapter,

    /// input or output URL. Unlike the old filename field, this field has no
    /// length restriction.
    ///
    /// - demuxing: set by `open_input`, initialized to an empty
    ///             string if url parameter was NULL in `open_input`.
    /// - muxing: may be set by the caller before calling avformat_write_header()
    ///           (or avformat_init_output() if that is called first) to a string
    ///           which is freeable by av_free(). Set to an empty string if it
    ///           was NULL in avformat_init_output().
    ///
    /// Freed by libavformat in avformat_free_context().
    url: [*:0]u8,

    /// Position of the first frame of the component, in
    /// AV_TIME_BASE fractional seconds. NEVER set this value directly:
    /// It is deduced from the AVStream values.
    ///
    /// Demuxing only, set by libavformat.
    start_time: i64,

    /// Duration of the stream, in AV_TIME_BASE fractional
    /// seconds. Only set this value if you know none of the individual stream
    /// durations and also do not set any of them. This is deduced from the
    /// AVStream values if not set.
    ///
    /// Demuxing only, set by libavformat.
    duration: i64,

    /// Total stream bitrate in bit/s, 0 if not
    /// available. Never set it directly if the file_size and the
    /// duration are known as FFmpeg can compute it automatically.
    bit_rate: i64,

    packet_size: c_uint,
    max_delay: c_int,

    /// Flags modifying the (de)muxer behaviour. A combination of AVFMT_FLAG_*.
    /// Set by the user before `open_input` / avformat_write_header().
    flags: c_int,
    /// Maximum number of bytes read from input in order to determine stream
    /// properties. Used when reading the global header and in
    /// avformat_find_stream_info().
    ///
    /// Demuxing only, set by the caller before `open_input`.
    ///
    /// @note this is \e not  used for determining the \ref AVInputFormat
    ///       "input format"
    /// @sa format_probesize
    probesize: i64,
    /// Maximum duration (in AV_TIME_BASE units) of the data read
    /// from input in avformat_find_stream_info().
    /// Demuxing only, set by the caller before avformat_find_stream_info().
    /// Can be set to 0 to let avformat choose using a heuristic.
    max_analyze_duration: i64,
    key: [*]const u8,
    keylen: c_int,
    nb_programs: c_uint,
    programs: [*]*Program,
    /// Forced video codec_id.
    /// Demuxing: Set by user.
    video_codec_id: Codec.ID,
    /// Forced audio codec_id.
    /// Demuxing: Set by user.
    audio_codec_id: Codec.ID,
    /// Forced subtitle codec_id.
    /// Demuxing: Set by user.
    subtitle_codec_id: Codec.ID,
    /// Forced Data codec_id.
    /// Demuxing: Set by user.
    data_codec_id: Codec.ID,
    /// Metadata that applies to the whole file.
    ///
    /// - demuxing: set by libavformat in `open_input`
    /// - muxing: may be set by the caller before avformat_write_header()
    ///
    /// Freed by libavformat in avformat_free_context().
    metadata: Dictionary.Mutable,
    /// Start time of the stream in real world time, in microseconds
    /// since the Unix epoch (00:00 1st January 1970). That is, pts=0 in the
    /// stream was captured at this real world time.
    /// - muxing: Set by the caller before avformat_write_header(). If set to
    ///           either 0 or AV_NOPTS_VALUE, then the current wall-time will
    ///           be used.
    /// - demuxing: Set by libavformat. AV_NOPTS_VALUE if unknown. Note that
    ///             the value may become known after some number of frames
    ///             have been received.
    start_time_realtime: i64,
    /// The number of frames used for determining the framerate in
    /// avformat_find_stream_info().
    /// Demuxing only, set by the caller before avformat_find_stream_info().
    fps_probe_size: c_int,
    /// Error recognition; higher values will detect more errors but may
    /// misdetect some more or less valid parts as errors.
    /// Demuxing only, set by the caller before `open_input`.
    error_recognition: c_int,
    /// Custom interrupt callbacks for the I/O layer.
    ///
    /// demuxing: set by the user before `open_input`.
    /// muxing: set by the user before avformat_write_header()
    /// (mainly useful for AVFMT_NOFILE formats). The callback
    /// should also be passed to avio_open2() if it's used to
    /// open the file.
    interrupt_callback: IOInterruptCB,
    /// Flags to enable debugging.
    debug: c_int,
    /// The maximum number of streams.
    /// - encoding: unused
    /// - decoding: set by user
    max_streams: c_int,
    /// Maximum amount of memory in bytes to use for the index of each stream.
    /// If the index exceeds this size, entries will be discarded as
    /// needed to maintain a smaller size. This can lead to slower or less
    /// accurate seeking (depends on demuxer).
    /// Demuxers for which a full in-memory index is mandatory will ignore
    /// this.
    /// - muxing: unused
    /// - demuxing: set by user
    max_index_size: c_uint,
    /// Maximum amount of memory in bytes to use for buffering frames
    /// obtained from realtime capture devices.
    max_picture_buffer: c_uint,
    /// Maximum buffering duration for interleaving.
    ///
    /// To ensure all the streams are interleaved correctly,
    /// av_interleaved_write_frame() will wait until it has at least one packet
    /// for each stream before actually writing any packets to the output file.
    /// When some streams are "sparse" (i.e. there are large gaps between
    /// successive packets), this can result in excessive buffering.
    ///
    /// This field specifies the maximum difference between the timestamps of the
    /// first and the last packet in the muxing queue, above which libavformat
    /// will output a packet regardless of whether it has queued a packet for all
    /// the streams.
    ///
    /// Muxing only, set by the caller before avformat_write_header().
    max_interleave_delta: i64,
    /// Maximum number of packets to read while waiting for the first timestamp.
    /// Decoding only.
    max_ts_probe: c_int,
    /// Max chunk time in microseconds.
    /// Note, not all formats support this and unpredictable things may happen if it is used when not supported.
    /// - encoding: Set by user
    /// - decoding: unused
    max_chunk_duration: c_int,
    /// Max chunk size in bytes
    /// Note, not all formats support this and unpredictable things may happen if it is used when not supported.
    /// - encoding: Set by user
    /// - decoding: unused
    max_chunk_size: c_int,
    /// Maximum number of packets that can be probed
    /// - encoding: unused
    /// - decoding: set by user
    max_probe_packets: c_int,
    /// Allow non-standard and experimental extension
    /// See `Codec.Context.strict_std_compliance`
    strict_std_compliance: c_int,
    /// Flags indicating events happening on the file, a combination of
    /// AVFMT_EVENT_FLAG_*.
    ///
    /// - demuxing: may be set by the demuxer in `open_input`,
    ///   avformat_find_stream_info() and av_read_frame(). Flags must be cleared
    ///   by the user once the event has been handled.
    /// - muxing: may be set by the user after avformat_write_header() to
    ///   indicate a user-triggered event.  The muxer will clear the flags for
    ///   events it has handled in av_[interleaved]_write_frame().
    event_flags: c_int,
    /// Avoid negative timestamps during muxing.
    /// Any value of the AVFMT_AVOID_NEG_TS_* constants.
    /// Note, this works better when using av_interleaved_write_frame().
    /// - muxing: Set by user
    /// - demuxing: unused
    avoid_negative_ts: c_int,
    /// Audio preload in microseconds.
    /// Note, not all formats support this and unpredictable things may happen if it is used when not supported.
    /// - encoding: Set by user
    /// - decoding: unused
    audio_preload: c_int,
    /// forces the use of wallclock timestamps as pts/dts of packets
    /// This has undefined results in the presence of B frames.
    /// - encoding: unused
    /// - decoding: Set by user
    use_wallclock_as_timestamps: c_int,
    /// Skip duration calcuation in estimate_timings_from_pts.
    /// - encoding: unused
    /// - decoding: set by user
    skip_estimate_duration_from_pts: c_int,
    /// used to force AVIO_FLAG_DIRECT.
    /// - encoding: unused
    /// - decoding: Set by user
    avio_flags: c_int,
    /// The duration field can be estimated through various ways, and this field can be used
    /// to know how the duration was estimated.
    /// - encoding: unused
    /// - decoding: Read by user
    duration_estimation_method: DurationEstimationMethod,
    /// Skip initial bytes when opening stream
    /// - encoding: unused
    /// - decoding: Set by user
    skip_initial_bytes: i64,
    /// Correct single timestamp overflows
    /// - encoding: unused
    /// - decoding: Set by user
    correct_ts_overflow: c_uint,
    /// Force seeking to any (also non key) frames.
    /// - encoding: unused
    /// - decoding: Set by user
    seek2any: c_int,
    /// Flush the I/O context after each packet.
    /// - encoding: Set by user
    /// - decoding: unused
    flush_packets: c_int,
    /// format probing score.
    ///
    /// The maximal score is AVPROBE_SCORE_MAX, its set when the demuxer probes
    /// the format.
    /// - encoding: unused
    /// - decoding: set by avformat, read by user
    probe_score: c_int,
    /// Maximum number of bytes read from input in order to identify the
    /// \ref AVInputFormat "input format". Only used when the format is not set
    /// explicitly by the caller.
    ///
    /// Demuxing only, set by the caller before `open_input`.
    ///
    /// @sa probesize
    format_probesize: c_int,
    /// ',' separated list of allowed decoders.
    /// If NULL then all are allowed
    /// - encoding: unused
    /// - decoding: set by user
    codec_whitelist: [*:0]u8,
    /// ',' separated list of allowed demuxers.
    /// If NULL then all are allowed
    /// - encoding: unused
    /// - decoding: set by user
    format_whitelist: [*:0]u8,
    /// ',' separated list of allowed protocols.
    /// - encoding: unused
    /// - decoding: set by user
    protocol_whitelist: [*:0]u8,
    /// ',' separated list of disallowed protocols.
    /// - encoding: unused
    /// - decoding: set by user
    protocol_blacklist: [*:0]u8,
    /// IO repositioned flag.
    /// This is set by avformat when the underlaying IO context read pointer
    /// is repositioned, for example when doing byte based seeking.
    /// Demuxers can use the flag to detect such changes.
    io_repositioned: c_int,
    /// Forced video codec.
    ///
    /// This allows forcing a specific decoder, even when there are multiple with
    /// the same codec_id.
    /// Demuxing: Set by user
    video_codec: *const Codec,
    /// Forced audio codec.
    ///
    /// This allows forcing a specific decoder, even when there are multiple with
    /// the same codec_id.
    /// Demuxing: Set by user
    audio_codec: *const Codec,
    /// Forced subtitle codec.
    ///
    /// This allows forcing a specific decoder, even when there are multiple with
    /// the same codec_id.
    /// Demuxing: Set by user
    subtitle_codec: *const Codec,
    /// Forced data codec.
    ///
    /// This allows forcing a specific decoder, even when there are multiple with
    /// the same codec_id.
    /// Demuxing: Set by user
    data_codec: *const Codec,
    /// Number of bytes to be written as padding in a metadata header.
    ///
    /// Demuxing: Unused.
    /// Muxing: Set by user via av_format_set_metadata_header_padding.
    metadata_header_padding: c_int,
    /// User data.
    /// This is a place for some private data of the user.
    @"opaque": ?*anyopaque,
    /// Callback used by devices to communicate with application.
    control_message_cb: av_format_control_message,
    /// Output timestamp offset, in microseconds.
    /// Muxing: set by user
    output_ts_offset: i64,
    /// dump format separator.
    /// can be ", " or "\n      " or anything else
    /// - muxing: Set by user.
    /// - demuxing: Set by user.
    dump_separator: *u8,
    /// A callback for opening new IO streams.
    ///
    /// Whenever a muxer or a demuxer needs to open an IO stream (typically from
    /// `open_input` for demuxers, but for certain formats can happen at
    /// other times as well), it will call this callback to obtain an IO context.
    ///
    /// @param s the format context
    /// @param pb on success, the newly opened IO context should be returned here
    /// @param url the url to open
    /// @param flags a combination of AVIO_FLAG_*
    /// @param options a dictionary of additional options, with the same
    ///                semantics as in avio_open2()
    /// @return 0 on success, a negative AVERROR code on failure
    ///
    /// @note Certain muxers and demuxers do nesting, i.e. they open one or more
    /// additional internal format contexts. Thus the AVFormatContext pointer
    /// passed to this callback may be different from the one facing the caller.
    /// It will, however, have the same 'opaque' field.
    io_open: ?*const fn (*FormatContext, **IOContext, [*]const u8, c_int, *Dictionary.Mutable) callconv(.c) c_int,
    /// A callback for closing the streams opened with AVFormatContext.io_open().
    ///
    /// Using this is preferred over io_close, because this can return an error.
    /// Therefore this callback is used instead of io_close by the generic
    /// libavformat code if io_close is NULL or the default.
    ///
    /// @param s the format context
    /// @param pb IO context to be closed and freed
    /// @return 0 on success, a negative AVERROR code on failure
    io_close2: ?*const fn (*FormatContext, *IOContext) callconv(.c) c_int,

    /// `free` can be used to free the context and everything
    /// allocated by the framework within it.
    pub fn alloc() error{OutOfMemory}!*FormatContext {
        return avformat_alloc_context() orelse return error.OutOfMemory;
    }

    // Free a `FormatContext` and all its streams.
    pub const free = avformat_free_context;

    /// Open an input stream and read the header.
    ///
    /// The codecs are not opened. The stream must be closed with
    /// `close_input`.
    pub fn open_input(
        /// URL of the stream to open.
        url: [*:0]const u8,
        /// If non-NULL, this parameter forces a specific input format.
        /// Otherwise the format is autodetected.
        fmt: ?*const InputFormat,
        /// A dictionary filled with `FormatContext` and demuxer-private
        /// options.
        ///
        /// On return this parameter will be destroyed and replaced with
        /// a dict containing options that were not found. May be NULL.
        options: ?*Dictionary.Mutable,
        pb: ?*IOContext,
    ) Error!*FormatContext {
        var ps: ?*FormatContext = try alloc();
        ps.?.pb = pb;
        // avformat_open_input takes ownership of the allocation.
        _ = try wrap(avformat_open_input(&ps, url, fmt, options));
        return ps.?;
    }

    /// Close an opened input `FormatContext`. Free it and all its contents.
    pub fn close_input(s: *FormatContext) void {
        var keep_your_dirty_hands_off_my_pointers_ffmpeg: ?*FormatContext = s;
        avformat_close_input(&keep_your_dirty_hands_off_my_pointers_ffmpeg);
    }

    /// Read packets of a media file to get stream information.
    ///
    /// This is useful for file formats with no headers such as MPEG. This
    /// function also computes the real framerate in case of MPEG-2 repeat
    /// frame mode.
    ///
    /// The logical file position is not changed by this function; examined
    /// packets may be buffered for later processing.
    ///
    /// This function isn't guaranteed to open all the codecs, so
    /// options being non-empty at return is a perfectly normal behavior.
    ///
    /// Does not let the user decide somehow what information is needed;
    /// sometimes wastes time getting stuff the user does not need.
    pub fn find_stream_info(
        ic: *FormatContext,
        /// If non-NULL, an ic.nb_streams long array of pointers to
        /// dictionaries, where i-th member contains options for codec
        /// corresponding to i-th stream.
        ///
        /// On return each dictionary will be filled with options that were not found.
        options: ?[*]Dictionary.Mutable,
    ) Error!void {
        _ = try wrap(avformat_find_stream_info(ic, options));
    }

    /// Find the "best" stream in the file.
    ///
    /// The best stream is determined according to various heuristics as the most
    /// likely to be what the user expects.
    ///
    /// If the decoder parameter is non-NULL, `find_best_stream` will find the
    /// default decoder for the stream's codec; streams for which no decoder can
    /// be found are ignored.
    ///
    /// Returns the non-negative stream number in case of success,
    /// AVERROR_STREAM_NOT_FOUND if no stream with the requested type could be
    /// found, AVERROR_DECODER_NOT_FOUND if streams were found but no decoder
    ///
    pub fn find_best_stream(
        ic: *FormatContext,
        /// stream type: video, audio, subtitles, etc.
        media_type: MediaType,
        /// user-requested stream number, or -1 for automatic selection
        wanted_stream_nb: c_int,
        /// try to find a stream related (eg. in the same program) to this one,
        /// or -1 if none
        related_stream: c_int,
    ) Error!struct { c_uint, *const Codec } {
        var decoder: ?*const Codec = undefined;
        const n = try wrap(av_find_best_stream(ic, media_type, wanted_stream_nb, related_stream, &decoder, 0));
        return .{ n, decoder.? };
    }

    /// Return the next frame of a stream.
    ///
    /// This function returns what is stored in the file, and does not validate
    /// that what is there are valid frames for the decoder. It will split what
    /// is stored in the file into frames and return one for each call. It will
    /// not omit invalid data between valid frames so as to give the decoder
    /// the maximum information possible for decoding.
    ///
    /// On success, the returned packet is reference-counted (pkt->buf is set)
    /// and valid indefinitely. The packet must be freed with av_packet_unref()
    /// when it is no longer needed. For video, the packet contains exactly one
    /// frame. For audio, it contains an integer number of frames if each frame
    /// has a known fixed size (e.g. PCM or ADPCM data). If the audio frames
    /// have a variable size (e.g. MPEG audio), then it contains one frame.
    ///
    /// pkt->pts, pkt->dts and pkt->duration are always set to correct values
    /// in `Stream.time_base` units (and guessed if the format cannot provide
    /// them). pkt->pts can be `NOPTS_VALUE` if the video format has B-frames,
    /// so it is better to rely on pkt->dts if you do not decompress the
    /// payload.
    ///
    /// Returns 0 if OK, < 0 on error or end of file. On error, pkt will be
    /// blank (as if it came from `Packet.alloc`).
    ///
    /// `pkt` will be initialized, so it may be uninitialized, but it must not
    /// contain data that needs to be freed.
    pub fn read_frame(s: *FormatContext, pkt: *Packet) Error!void {
        _ = try wrap(av_read_frame(s, pkt));
    }

    /// Seek to the keyframe at timestamp in the specified stream.
    pub fn seek_frame(
        /// media file handle
        s: *FormatContext,
        /// If stream_index is (-1), a default stream is selected, and
        /// timestamp is automatically converted from AV_TIME_BASE units to the
        /// stream specific time_base.
        stream_index: c_int,
        /// In AVStream.time_base units or, if no stream is specified, in
        /// AV_TIME_BASE units.
        timestamp: i64,
        /// select direction and seeking mode
        flags: c_int,
    ) Error!void {
        _ = try wrap(av_seek_frame(s, stream_index, timestamp, flags));
    }

    /// Discard all internally buffered data. This can be useful when dealing with
    /// discontinuities in the byte stream. Generally works only with formats that
    /// can resync. This includes headerless formats like MPEG-TS/TS but should also
    /// work with NUT, Ogg and in a limited way AVI for example.
    ///
    /// The set of streams, the detected duration, stream parameters and codecs do
    /// not change when calling this function. If you want a complete reset, it's
    /// better to open a new `FormatContext`.
    ///
    /// This does not flush the `IOContext` (s->pb). If necessary, call
    /// avio_flush(s->pb) before calling this function.
    ///
    /// @return >=0 on success, error code otherwise
    pub fn flush(s: *FormatContext) Error!void {
        _ = try wrap(avformat_flush(s));
    }

    /// Print detailed information about the input or output format, such as
    /// duration, bitrate, streams, container, programs, metadata, side data,
    /// codec and time base.
    pub const dump = av_dump_format;
};

pub const Class = extern struct {
    class_name: [*c]const u8,
    item_name: ?*const fn (?*anyopaque) callconv(.c) [*c]const u8,
    option: ?*const Option_6,
    version: c_int,
    log_level_offset_offset: c_int,
    parent_log_context_offset: c_int,
    category: ClassCategory,
    get_category: ?*const fn (?*anyopaque) callconv(.c) ClassCategory,
    query_ranges: ?*const fn ([*c]?*OptionRanges, ?*anyopaque, [*c]const u8, c_int) callconv(.c) c_int,
    child_next: ?*const fn (?*anyopaque, ?*anyopaque) callconv(.c) ?*anyopaque,
    child_class_iterate: ?*const fn ([*c]?*anyopaque) callconv(.c) [*c]const Class,
};

pub const InputFormat = extern struct {
    name: [*c]const u8,
    long_name: [*c]const u8,
    flags: c_int,
    extensions: [*c]const u8,
    codec_tag: [*c]const ?*const CodecTag,
    priv_class: [*c]const Class,
    mime_type: [*c]const u8,
    raw_codec_id: c_int,
    priv_data_size: c_int,
    flags_internal: c_int,
    read_probe: ?*const fn ([*c]const ProbeData) callconv(.c) c_int,
    read_header: ?*const fn ([*c]FormatContext) callconv(.c) c_int,
    read_packet: ?*const fn ([*c]FormatContext, [*c]Packet) callconv(.c) c_int,
    read_close: ?*const fn ([*c]FormatContext) callconv(.c) c_int,
    read_seek: ?*const fn ([*c]FormatContext, c_int, i64, c_int) callconv(.c) c_int,
    read_timestamp: ?*const fn ([*c]FormatContext, c_int, [*c]i64, i64) callconv(.c) i64,
    read_play: ?*const fn ([*c]FormatContext) callconv(.c) c_int,
    read_pause: ?*const fn ([*c]FormatContext) callconv(.c) c_int,
    read_seek2: ?*const fn ([*c]FormatContext, c_int, i64, i64, i64, c_int) callconv(.c) c_int,
    get_device_list: ?*const fn ([*c]FormatContext, ?*DeviceInfoList) callconv(.c) c_int,
};

pub const OutputFormat = extern struct {
    name: [*c]const u8,
    long_name: [*c]const u8,
    mime_type: [*c]const u8,
    extensions: [*c]const u8,
    audio_codec: Codec.ID,
    video_codec: Codec.ID,
    subtitle_codec: Codec.ID,
    flags: c_int,
    codec_tag: [*c]const ?*const CodecTag,
    priv_class: [*c]const Class,
};

pub const IOContext = extern struct {
    av_class: *const Class,
    buffer: [*]u8,
    buffer_size: c_int,
    buf_ptr: [*]u8,
    buf_end: [*]u8,
    @"opaque": ?*anyopaque,
    read_packet: ?*const fn (?*anyopaque, [*c]u8, c_int) callconv(.c) c_int,
    write_packet: ?*const fn (?*anyopaque, [*c]const u8, c_int) callconv(.c) c_int,
    seek: ?*const fn (?*anyopaque, i64, SEEK) callconv(.c) i64,
    pos: i64,
    eof_reached: c_int,
    @"error": c_int,
    write_flag: c_int,
    max_packet_size: c_int,
    min_packet_size: c_int,
    checksum: c_ulong,
    checksum_ptr: [*c]u8,
    update_checksum: ?*const fn (c_ulong, [*c]const u8, c_uint) callconv(.c) c_ulong,
    read_pause: ?*const fn (?*anyopaque, c_int) callconv(.c) c_int,
    read_seek: ?*const fn (?*anyopaque, c_int, i64, c_int) callconv(.c) i64,
    seekable: c_int,
    direct: c_int,
    protocol_whitelist: [*c]const u8,
    protocol_blacklist: [*c]const u8,
    write_data_type: ?*const fn (?*anyopaque, [*c]const u8, c_int, IODataMarkerType, i64) callconv(.c) c_int,
    ignore_boundary_point: c_int,
    buf_ptr_max: [*c]u8,
    bytes_read: i64,
    bytes_written: i64,

    pub const WriteFlag = enum(c_int) {
        read_only = 0,
        writable = 1,
    };

    /// Allocate and initialize an `IOContext` for buffered I/O. It must be later
    /// freed with `free`.
    pub fn alloc(
        /// Memory block for input/output operations via AVIOContext.
        /// The buffer must be allocated with av_malloc() and friends.
        /// It may be freed and replaced with a new buffer by libavformat.
        /// `IOContext.buffer` holds the buffer currently in use,
        /// which must be later freed with av_free().
        ///
        /// The buffer size is very important for performance.
        /// For protocols with fixed blocksize it should be set to this blocksize.
        /// For others a typical size is a cache page, e.g. 4kb.
        buffer: []u8,
        /// Whether the buffer should be writable.
        write_flag: WriteFlag,
        /// An opaque pointer to user-specific data.
        userdata: ?*anyopaque,
        /// A function for refilling the buffer.
        ///
        /// For stream protocols, must never return 0 but rather a proper AVERROR code.
        read_packet: ?*const fn (?*anyopaque, [*:0]u8, c_int) callconv(.c) c_int,
        /// A function for writing the buffer contents.
        ///
        /// The function may not change the input buffers content.
        write_packet: ?*const fn (?*anyopaque, [*:0]u8, c_int) callconv(.c) c_int,
        /// A function for seeking to specified byte position.
        seek: ?*const fn (?*anyopaque, i64, SEEK) callconv(.c) i64,
    ) error{OutOfMemory}!*IOContext {
        return avio_alloc_context(
            buffer.ptr,
            @intCast(buffer.len),
            write_flag,
            userdata,
            read_packet,
            write_packet,
            seek,
        ) orelse return error.OutOfMemory;
    }

    pub fn free(ioc: *IOContext) void {
        var keep_your_dirty_hands_off_my_pointers_ffmpeg: ?*IOContext = ioc;
        avio_context_free(&keep_your_dirty_hands_off_my_pointers_ffmpeg);
    }

    /// Close the resource accessed by the IOContext s and free it.
    ///
    /// This function can only be used if s was opened by avio_open().
    ///
    /// The internal buffer is automatically flushed before closing the
    /// resource.
    pub fn close(s: *IOContext) Error!void {
        _ = try wrap(avio_close(s));
    }
};

pub const Stream = extern struct {
    av_class: *const Class,
    index: c_int,
    id: c_int,
    codecpar: *Codec.Parameters,
    priv_data: ?*anyopaque,
    time_base: Rational,
    start_time: i64,
    duration: i64,
    nb_frames: i64,
    disposition: c_int,
    discard: Discard,
    sample_aspect_ratio: Rational,
    metadata: Dictionary.Mutable,
    avg_frame_rate: Rational,
    attached_pic: Packet,
    side_data: [*]PacketSideData,
    nb_side_data: c_int,
    event_flags: c_int,
    r_frame_rate: Rational,
    pts_wrap_bits: c_int,
};

pub const Program = extern struct {
    id: c_int,
    flags: c_int,
    discard: Discard,
    stream_index: [*]c_uint,
    nb_stream_indexes: c_uint,
    metadata: Dictionary.Mutable,
    program_num: c_int,
    pmt_pid: c_int,
    pcr_pid: c_int,
    pmt_version: c_int,
    start_time: i64,
    end_time: i64,
    pts_wrap_reference: i64,
    pts_wrap_behavior: c_int,
};

pub const Chapter = extern struct {
    id: i64,
    time_base: Rational,
    start: i64,
    end: i64,
    metadata: Dictionary.Mutable,
};

pub const Dictionary = opaque {
    pub const Const = extern struct {
        dict: ?*const Dictionary,

        pub const empty: Const = .{ .dict = null };

        /// Get a dictionary entry with matching key.
        ///
        /// The returned entry key or value must not be changed, or it will
        /// cause undefined behavior.
        pub const get = av_dict_get;

        /// Iterates through all entries in the dictionary.
        ///
        /// The returned `Entry` key/value must not be changed.
        ///
        /// As set() invalidates all previous entries returned by this function,
        /// it must not be called while iterating over the dict.
        pub const iterate = av_dict_iterate;

        /// Get number of entries in dictionary.
        pub const count = av_dict_count;

        /// Free all the memory allocated for an Dictionary struct and all keys
        /// and values.
        pub fn free(dict: Const) void {
            var keep_your_dirty_hands_off_my_pointers_ffmpeg = dict;
            av_dict_free(&keep_your_dirty_hands_off_my_pointers_ffmpeg);
        }
    };

    pub const Mutable = extern struct {
        dict: ?*Dictionary,

        pub const empty: Mutable = .{ .dict = null };

        pub fn toConst(dict: Mutable) Const {
            return .{ .dict = dict.dict };
        }

        /// Get a dictionary entry with matching key.
        ///
        /// The returned entry key or value must not be changed, or it will
        /// cause undefined behavior.
        pub fn get(dict: Mutable, key: [*:0]const u8, prev: ?*const Entry, flags: Flags) ?*const Entry {
            return dict.toConst().get(key, prev, flags);
        }

        /// Iterates through all entries in the dictionary.
        ///
        /// The returned `Entry` key/value must not be changed.
        ///
        /// As set() invalidates all previous entries returned by this function,
        /// it must not be called while iterating over the dict.
        pub fn iterate(dict: Mutable, prev: ?*const Entry) ?*const Entry {
            return dict.toConst().iterate(prev);
        }

        /// Get number of entries in dictionary.
        pub fn count(dict: Mutable) c_int {
            return dict.toConst().count();
        }

        /// Set the given entry in *pm, overwriting an existing entry.
        ///
        /// If DONT_STRDUP_KEY or DONT_STRDUP_VAL is set, these arguments will be
        /// freed on error.
        ///
        /// Adding a new entry to a dictionary invalidates all existing entries
        /// previously returned with get() or iterate().
        pub fn set(dict: *Mutable, key: [*:0]const u8, value: ?[*:0]const u8, flags: Flags) error{OutOfMemory}!void {
            _ = wrap(av_dict_set(dict, key, value, flags)) catch |err| switch (err) {
                error.FFmpegInvalid => unreachable, // Zig prevents this by not making `key` nullable.
                error.OutOfMemory => |e| return e,
                else => unreachable, // I checked the source code, those are the only possible errors.
            };
        }

        /// Set the given entry in *pm, overwriting an existing entry.
        ///
        /// If DONT_STRDUP_KEY or DONT_STRDUP_VAL is set, these arguments will be
        /// freed on error.
        ///
        /// Adding a new entry to a dictionary invalidates all existing entries
        /// previously returned with get() or iterate().
        pub fn set_int(dict: *Mutable, key: [*:0]const u8, value: i64, flags: Flags) error{OutOfMemory}!void {
            _ = wrap(av_dict_set_int(dict, key, value, flags)) catch |err| switch (err) {
                error.FFmpegInvalid => unreachable, // Zig prevents this by not making `key` nullable.
                error.OutOfMemory => |e| return e,
                else => unreachable, // I checked the source code, those are the only possible errors.
            };
        }

        pub fn copy(dst: *Mutable, src: Const, flags: Flags) error{OutOfMemory}!void {
            _ = wrap(av_dict_copy(dst, src, flags)) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => unreachable, // I checked the source code, those are the only possible errors.
            };
        }

        /// Free all the memory allocated for an Dictionary struct and all keys
        /// and values.
        pub fn free(dict: Mutable) void {
            dict.toConst().free();
        }
    };

    /// Flags that influence behavior of the matching of keys or insertion to the dictionary.
    pub const Flags = packed struct(c_int) {
        /// Only get an entry with exact-case key match. Only relevant in get().
        MATCH_CASE: bool = false,
        /// Return first entry in a dictionary whose first part corresponds to
        /// the search key.
        IGNORE_SUFFIX: bool = false,
        /// Take ownership of a key that's been allocated with av_malloc() or
        /// another memory allocation function.
        DONT_STRDUP_KEY: bool = false,
        /// Take ownership of a value that's been allocated with av_malloc() or
        /// another memory allocation function.
        DONT_STRDUP_VAL: bool = false,
        /// Don't overwrite existing entries.
        DONT_OVERWRITE: bool = false,
        /// If the entry already exists, append to it.  Note that no delimiter
        /// is added, the strings are simply concatenated.
        APPEND: bool = false,
        /// Allow to store several equal keys in the dictionary.
        MULTIKEY: bool = false,
        unused: @Type(.{ .int = .{ .signedness = .unsigned, .bits = @bitSizeOf(c_int) - 7 } }) = 0,
    };

    pub const Entry = extern struct {
        key: [*:0]u8,
        value: [*:0]u8,
    };
};

pub const IOInterruptCB = extern struct {
    callback: ?*const fn (?*anyopaque) callconv(.c) c_int,
    @"opaque": ?*anyopaque,
};

pub const DurationEstimationMethod = enum(c_uint) {
    PTS = 0,
    STREAM = 1,
    BITRATE = 2,
};

pub const av_format_control_message = ?*const fn ([*c]FormatContext, c_int, ?*anyopaque, usize) callconv(.c) c_int;

pub const Option_6 = opaque {};

pub const ClassCategory = enum(c_uint) {
    NA = 0,
    INPUT = 1,
    OUTPUT = 2,
    MUXER = 3,
    DEMUXER = 4,
    ENCODER = 5,
    DECODER = 6,
    FILTER = 7,
    BITSTREAM_FILTER = 8,
    SWSCALER = 9,
    SWRESAMPLER = 10,
    DEVICE_VIDEO_OUTPUT = 40,
    DEVICE_VIDEO_INPUT = 41,
    DEVICE_AUDIO_OUTPUT = 42,
    DEVICE_AUDIO_INPUT = 43,
    DEVICE_OUTPUT = 44,
    DEVICE_INPUT = 45,
};

pub const OptionRanges = opaque {};
pub const CodecTag = opaque {};

pub const ProbeData = extern struct {
    filename: [*c]const u8,
    buf: [*c]u8,
    buf_size: c_int,
    mime_type: [*c]const u8,
};

pub const Packet = extern struct {
    buf: *BufferRef,
    pts: i64,
    dts: i64,
    data: [*]u8,
    size: c_int,
    stream_index: c_int,
    flags: c_int,
    side_data: [*]PacketSideData,
    side_data_elems: c_int,
    duration: i64,
    pos: i64,
    @"opaque": ?*anyopaque,
    opaque_ref: *BufferRef,
    time_base: Rational,

    pub fn alloc() error{OutOfMemory}!*Packet {
        return av_packet_alloc() orelse return error.OutOfMemory;
    }

    pub fn free(p: *Packet) void {
        var keep_your_dirty_hands_off_my_pointers_ffmpeg: ?*Packet = p;
        av_packet_free(&keep_your_dirty_hands_off_my_pointers_ffmpeg);
    }

    /// Wipe the packet.
    ///
    /// Unreference the buffer referenced by the packet and reset the
    /// remaining packet fields to their default values.
    pub const unref = av_packet_unref;
};

pub const DeviceInfoList = opaque {};

pub const IODataMarkerType = enum(c_uint) {
    HEADER = 0,
    SYNC_POINT = 1,
    BOUNDARY_POINT = 2,
    UNKNOWN = 3,
    TRAILER = 4,
    FLUSH_POINT = 5,
};

pub const Rational = extern struct {
    num: c_int,
    den: c_int,

    pub fn q2d(a: Rational) f64 {
        const num: f64 = @floatFromInt(a.num);
        const den: f64 = @floatFromInt(a.den);
        return num / den;
    }
};

pub const Discard = enum(c_int) {
    NONE = -16,
    DEFAULT = 0,
    NONREF = 8,
    BIDIR = 16,
    NONINTRA = 24,
    NONKEY = 32,
    ALL = 48,
};

pub const PacketSideData = extern struct {
    data: [*c]u8,
    size: usize,
    type: PacketSideDataType,
};

pub const MediaType = enum(c_int) {
    UNKNOWN = -1,
    VIDEO = 0,
    AUDIO = 1,
    DATA = 2,
    SUBTITLE = 3,
    ATTACHMENT = 4,
};

pub const PixelFormat = enum(c_int) {
    NONE = -1,
    YUV420P = 0,
    YUYV422 = 1,
    RGB24 = 2,
    BGR24 = 3,
    YUV422P = 4,
    YUV444P = 5,
    YUV410P = 6,
    YUV411P = 7,
    GRAY8 = 8,
    MONOWHITE = 9,
    MONOBLACK = 10,
    PAL8 = 11,
    YUVJ420P = 12,
    YUVJ422P = 13,
    YUVJ444P = 14,
    UYVY422 = 15,
    UYYVYY411 = 16,
    BGR8 = 17,
    BGR4 = 18,
    BGR4_BYTE = 19,
    RGB8 = 20,
    RGB4 = 21,
    RGB4_BYTE = 22,
    NV12 = 23,
    NV21 = 24,
    ARGB = 25,
    RGBA = 26,
    ABGR = 27,
    BGRA = 28,
    GRAY16BE = 29,
    GRAY16LE = 30,
    YUV440P = 31,
    YUVJ440P = 32,
    YUVA420P = 33,
    RGB48BE = 34,
    RGB48LE = 35,
    RGB565BE = 36,
    RGB565LE = 37,
    RGB555BE = 38,
    RGB555LE = 39,
    BGR565BE = 40,
    BGR565LE = 41,
    BGR555BE = 42,
    BGR555LE = 43,
    VAAPI = 44,
    YUV420P16LE = 45,
    YUV420P16BE = 46,
    YUV422P16LE = 47,
    YUV422P16BE = 48,
    YUV444P16LE = 49,
    YUV444P16BE = 50,
    DXVA2_VLD = 51,
    RGB444LE = 52,
    RGB444BE = 53,
    BGR444LE = 54,
    BGR444BE = 55,
    /// Y400A = 56,
    /// GRAY8A = 56,
    YA8 = 56,
    BGR48BE = 57,
    BGR48LE = 58,
    YUV420P9BE = 59,
    YUV420P9LE = 60,
    YUV420P10BE = 61,
    YUV420P10LE = 62,
    YUV422P10BE = 63,
    YUV422P10LE = 64,
    YUV444P9BE = 65,
    YUV444P9LE = 66,
    YUV444P10BE = 67,
    YUV444P10LE = 68,
    YUV422P9BE = 69,
    YUV422P9LE = 70,
    /// GBR24P = 71,
    GBRP = 71,
    GBRP9BE = 72,
    GBRP9LE = 73,
    GBRP10BE = 74,
    GBRP10LE = 75,
    GBRP16BE = 76,
    GBRP16LE = 77,
    YUVA422P = 78,
    YUVA444P = 79,
    YUVA420P9BE = 80,
    YUVA420P9LE = 81,
    YUVA422P9BE = 82,
    YUVA422P9LE = 83,
    YUVA444P9BE = 84,
    YUVA444P9LE = 85,
    YUVA420P10BE = 86,
    YUVA420P10LE = 87,
    YUVA422P10BE = 88,
    YUVA422P10LE = 89,
    YUVA444P10BE = 90,
    YUVA444P10LE = 91,
    YUVA420P16BE = 92,
    YUVA420P16LE = 93,
    YUVA422P16BE = 94,
    YUVA422P16LE = 95,
    YUVA444P16BE = 96,
    YUVA444P16LE = 97,
    VDPAU = 98,
    XYZ12LE = 99,
    XYZ12BE = 100,
    NV16 = 101,
    NV20LE = 102,
    NV20BE = 103,
    RGBA64BE = 104,
    RGBA64LE = 105,
    BGRA64BE = 106,
    BGRA64LE = 107,
    YVYU422 = 108,
    YA16BE = 109,
    YA16LE = 110,
    GBRAP = 111,
    GBRAP16BE = 112,
    GBRAP16LE = 113,
    QSV = 114,
    MMAL = 115,
    D3D11VA_VLD = 116,
    CUDA = 117,
    @"0RGB" = 118,
    RGB0 = 119,
    @"0BGR" = 120,
    BGR0 = 121,
    YUV420P12BE = 122,
    YUV420P12LE = 123,
    YUV420P14BE = 124,
    YUV420P14LE = 125,
    YUV422P12BE = 126,
    YUV422P12LE = 127,
    YUV422P14BE = 128,
    YUV422P14LE = 129,
    YUV444P12BE = 130,
    YUV444P12LE = 131,
    YUV444P14BE = 132,
    YUV444P14LE = 133,
    GBRP12BE = 134,
    GBRP12LE = 135,
    GBRP14BE = 136,
    GBRP14LE = 137,
    YUVJ411P = 138,
    BAYER_BGGR8 = 139,
    BAYER_RGGB8 = 140,
    BAYER_GBRG8 = 141,
    BAYER_GRBG8 = 142,
    BAYER_BGGR16LE = 143,
    BAYER_BGGR16BE = 144,
    BAYER_RGGB16LE = 145,
    BAYER_RGGB16BE = 146,
    BAYER_GBRG16LE = 147,
    BAYER_GBRG16BE = 148,
    BAYER_GRBG16LE = 149,
    BAYER_GRBG16BE = 150,
    XVMC = 151,
    YUV440P10LE = 152,
    YUV440P10BE = 153,
    YUV440P12LE = 154,
    YUV440P12BE = 155,
    AYUV64LE = 156,
    AYUV64BE = 157,
    VIDEOTOOLBOX = 158,
    P010LE = 159,
    P010BE = 160,
    GBRAP12BE = 161,
    GBRAP12LE = 162,
    GBRAP10BE = 163,
    GBRAP10LE = 164,
    MEDIACODEC = 165,
    GRAY12BE = 166,
    GRAY12LE = 167,
    GRAY10BE = 168,
    GRAY10LE = 169,
    P016LE = 170,
    P016BE = 171,
    D3D11 = 172,
    GRAY9BE = 173,
    GRAY9LE = 174,
    GBRPF32BE = 175,
    GBRPF32LE = 176,
    GBRAPF32BE = 177,
    GBRAPF32LE = 178,
    DRM_PRIME = 179,
    OPENCL = 180,
    GRAY14BE = 181,
    GRAY14LE = 182,
    GRAYF32BE = 183,
    GRAYF32LE = 184,
    YUVA422P12BE = 185,
    YUVA422P12LE = 186,
    YUVA444P12BE = 187,
    YUVA444P12LE = 188,
    NV24 = 189,
    NV42 = 190,
    VULKAN = 191,
    Y210BE = 192,
    Y210LE = 193,
    X2RGB10LE = 194,
    X2RGB10BE = 195,
    X2BGR10LE = 196,
    X2BGR10BE = 197,
    P210BE = 198,
    P210LE = 199,
    P410BE = 200,
    P410LE = 201,
    P216BE = 202,
    P216LE = 203,
    P416BE = 204,
    P416LE = 205,
    VUYA = 206,
    RGBAF16BE = 207,
    RGBAF16LE = 208,
    VUYX = 209,
    P012LE = 210,
    P012BE = 211,
    Y212BE = 212,
    Y212LE = 213,
    XV30BE = 214,
    XV30LE = 215,
    XV36BE = 216,
    XV36LE = 217,
    RGBF32BE = 218,
    RGBF32LE = 219,
    RGBAF32BE = 220,
    RGBAF32LE = 221,
    P212BE = 222,
    P212LE = 223,
    P412BE = 224,
    P412LE = 225,
    GBRAP14BE = 226,
    GBRAP14LE = 227,
};

pub const SampleFormat = enum(c_int) {
    NONE = -1,
    U8 = 0,
    S16 = 1,
    S32 = 2,
    FLT = 3,
    DBL = 4,
    U8P = 5,
    S16P = 6,
    S32P = 7,
    FLTP = 8,
    DBLP = 9,
    S64 = 10,
    S64P = 11,

    /// Return the name of sample_fmt, or NULL if sample_fmt is not recognized.
    pub const get_name = av_get_sample_fmt_name;

    /// Return number of bytes per sample, or zero if unknown.
    pub const get_bytes_per_sample = av_get_bytes_per_sample;

    /// Check if the sample format is planar.
    ///
    /// @param sample_fmt the sample format to inspect
    /// @return 1 if the sample format is planar, 0 if it is interleaved
    pub fn is_planar(sample_fmt: SampleFormat) bool {
        return av_sample_fmt_is_planar(sample_fmt) != 0;
    }
};

pub const Profile = extern struct {
    profile: c_int,
    name: ?[*:0]const u8,
};

/// A set of channels ordered in a specific way.
///
/// If the channel order is AV_CHANNEL_ORDER_UNSPEC, this struct carries only
/// the channel count.
///
/// All orders may be treated as if they were AV_CHANNEL_ORDER_UNSPEC by
/// ignoring everything but the channel count, as long as `check` considers
/// they are valid.
///
/// Unlike most structures in FFmpeg, `@sizeOf(ChannelLayout)` is a part of the
/// public ABI and may be used by the caller. E.g. it may be allocated on stack
/// or embedded in caller-defined structs.
///
/// Can be initialized as follows:
/// - default initialization with {0}, followed by setting all used fields
///   correctly;
/// - by assigning one of the predefined AV_CHANNEL_LAYOUT_* initializers;
/// - with a constructor function, such as `default`, `from_mask` or
///   `from_string`.
///
/// The channel layout must be unitialized with `uninit`.
///
/// Copying a `ChannelLayout` via assigning is forbidden; `copy` must be used
/// instead.
///
/// No new fields may be added to it without a major version bump, except for
/// new elements of the union fitting in `@sizeOf(u64)`.
pub const ChannelLayout = extern struct {
    /// This is a mandatory field.
    order: ChannelOrder,
    /// Number of channels in this layout. Mandatory field.
    nb_channels: c_int,
    /// Details about which channels are present in this layout.
    ///
    /// For `ChannelOrder.UNSPEC`, this field is undefined and must not be
    /// used.
    u: extern union {
        /// This member must be used for `ChannelOrder.NATIVE`, and may be used
        /// for `ChannelOrder.AMBISONIC` to signal non-diegetic channels.
        ///
        /// It is a bitmask, where the position of each set bit means that the
        /// AVChannel with the corresponding value is present.
        ///
        /// I.e. when (mask & (1 << AV_CHAN_FOO)) is non-zero, then AV_CHAN_FOO
        /// is present in the layout. Otherwise it is not present.
        ///
        /// When a channel layout using a bitmask is constructed or
        /// modified manually (i.e.  not using any of the av_channel_layout_*
        /// functions), the code doing it must ensure that the number of set
        /// bits is equal to nb_channels.
        mask: u64,

        /// This member must be used when the channel order is
        /// `ChannelOrder.CUSTOM`. It is a `nb_channels`-sized array, with each
        /// element signalling the presence of the `Channel` with the
        /// corresponding value in map[i].id.
        ///
        /// I.e. when map[i].id is equal to AV_CHAN_FOO, then AV_CH_FOO is the
        /// i-th channel in the audio data.
        ///
        /// When map[i].id is in the range between `Channel.AMBISONIC_BASE` and
        /// `Channel.AMBISONIC_END` (inclusive), the channel contains an ambisonic
        /// component with ACN index (as defined above)
        /// `n = map[i].id - Channel.AMBISONIC_BASE`.
        ///
        /// map[i].name may be filled with a 0-terminated string, in which case
        /// it will be used for the purpose of identifying the channel with the
        /// convenience functions below. Otherise it must be zeroed.
        map: [*]ChannelCustom,
    },
    /// For some private data of the user.
    @"opaque": ?*anyopaque,

    /// Check whether two channel layouts are semantically the same.
    ///
    /// i.e. the same channels are present on the same positions in both.
    ///
    /// If one of the channel layouts is `ChannelOrder.UNSPEC`, while the other
    /// is not, they are considered to be unequal. If both are
    /// `ChannelOrder.UNSPEC`, they are considered equal iff the channel counts
    /// are the same in both.
    ///
    /// Returns true if and only if they are equal.
    ///
    /// Asserts both channels are valid.
    /// @return 0 if chl and chl1 are equal, 1 if they are not equal. A negative
    ///         AVERROR code if one or both are invalid.
    pub fn compare(a: *const ChannelLayout, b: *const ChannelLayout) bool {
        return switch (av_channel_layout_compare(a, b)) {
            0 => true,
            1 => false,
            else => unreachable, // invalid channel layout
        };
    }

    /// Free any allocated data in the channel layout and reset the channel
    /// count to 0.
    pub const uninit = av_channel_layout_uninit;

    /// Get a human-readable string describing the channel layout properties.
    ///
    /// The string will be in the same format that is accepted by
    /// `from_string`, allowing to rebuild the same channel layout, except for
    /// opaque pointers.
    ///
    /// Asserts the channel layout is valid and `buf` is large enough to store
    /// the result.
    pub fn describe(
        cl: *const ChannelLayout,
        /// Pre-allocated buffer where to put the generated string.
        buf: []u8,
    ) [:0]u8 {
        const rc = av_channel_layout_describe(cl, buf.ptr, buf.len);
        std.debug.assert(rc >= 0); // invalid channel layout
        std.debug.assert(rc <= buf.len); // buffer too small
        return buf[0..@intCast(rc - 1) :0];
    }

    /// Initialize a native channel layout from a bitmask indicating which
    /// channels are present.
    ///
    /// Asserts the mask values are valid.
    pub fn from_mask(
        /// The layout structure to be initialized.
        channel_layout: *ChannelLayout,
        /// Bitmask describing the channel layout.
        mask: u64,
    ) void {
        _ = wrap(av_channel_layout_from_mask(channel_layout, mask)) catch unreachable;
    }
};

pub const BufferRef = extern struct {
    buffer: ?*Buffer,
    data: [*]u8,
    size: usize,
};

pub const FieldOrder = enum(c_uint) {
    UNKNOWN = 0,
    PROGRESSIVE = 1,
    TT = 2,
    BB = 3,
    TB = 4,
    BT = 5,
};

pub const ColorRange = enum(c_uint) {
    UNSPECIFIED = 0,
    MPEG = 1,
    JPEG = 2,
};

pub const ColorPrimaries = enum(c_uint) {
    RESERVED0 = 0,
    BT709 = 1,
    UNSPECIFIED = 2,
    RESERVED = 3,
    BT470M = 4,
    BT470BG = 5,
    SMPTE170M = 6,
    SMPTE240M = 7,
    FILM = 8,
    BT2020 = 9,
    /// SMPTEST428_1 = 10,
    SMPTE428 = 10,
    SMPTE431 = 11,
    SMPTE432 = 12,
    /// JEDEC_P22 = 22,
    EBU3213 = 22,
};

pub const ColorTransferCharacteristic = enum(c_uint) {
    RESERVED0 = 0,
    BT709 = 1,
    UNSPECIFIED = 2,
    RESERVED = 3,
    GAMMA22 = 4,
    GAMMA28 = 5,
    SMPTE170M = 6,
    SMPTE240M = 7,
    LINEAR = 8,
    LOG = 9,
    LOG_SQRT = 10,
    IEC61966_2_4 = 11,
    BT1361_ECG = 12,
    IEC61966_2_1 = 13,
    BT2020_10 = 14,
    BT2020_12 = 15,
    /// SMPTEST2084 = 16,
    SMPTE2084 = 16,
    /// SMPTEST428_1 = 17,
    SMPTE428 = 17,
    ARIB_STD_B67 = 18,
};

pub const ColorSpace = enum(c_uint) {
    RGB = 0,
    BT709 = 1,
    UNSPECIFIED = 2,
    RESERVED = 3,
    FCC = 4,
    BT470BG = 5,
    SMPTE170M = 6,
    SMPTE240M = 7,
    /// YCOCG = 8,
    YCGCO = 8,
    BT2020_NCL = 9,
    BT2020_CL = 10,
    SMPTE2085 = 11,
    CHROMA_DERIVED_NCL = 12,
    CHROMA_DERIVED_CL = 13,
    ICTCP = 14,
};

pub const ChromaLocation = enum(c_uint) {
    UNSPECIFIED = 0,
    LEFT = 1,
    CENTER = 2,
    TOPLEFT = 3,
    TOP = 4,
    BOTTOMLEFT = 5,
    BOTTOM = 6,
};

pub const PacketSideDataType = enum(c_uint) {
    PALETTE = 0,
    NEW_EXTRADATA = 1,
    PARAM_CHANGE = 2,
    H263_MB_INFO = 3,
    REPLAYGAIN = 4,
    DISPLAYMATRIX = 5,
    STEREO3D = 6,
    AUDIO_SERVICE_TYPE = 7,
    QUALITY_STATS = 8,
    FALLBACK_TRACK = 9,
    CPB_PROPERTIES = 10,
    SKIP_SAMPLES = 11,
    JP_DUALMONO = 12,
    STRINGS_METADATA = 13,
    SUBTITLE_POSITION = 14,
    MATROSKA_BLOCKADDITIONAL = 15,
    WEBVTT_IDENTIFIER = 16,
    WEBVTT_SETTINGS = 17,
    METADATA_UPDATE = 18,
    MPEGTS_STREAM_ID = 19,
    MASTERING_DISPLAY_METADATA = 20,
    SPHERICAL = 21,
    CONTENT_LIGHT_LEVEL = 22,
    A53_CC = 23,
    ENCRYPTION_INIT_INFO = 24,
    ENCRYPTION_INFO = 25,
    AFD = 26,
    PRFT = 27,
    ICC_PROFILE = 28,
    DOVI_CONF = 29,
    S12M_TIMECODE = 30,
    DYNAMIC_HDR10_PLUS = 31,
};

pub const ChannelOrder = enum(c_uint) {
    UNSPEC = 0,
    NATIVE = 1,
    CUSTOM = 2,
    AMBISONIC = 3,
};

pub const ChannelCustom = extern struct {
    id: Channel,
    name: [16]u8,
    @"opaque": ?*anyopaque,
};

pub const Buffer = opaque {};

pub const Channel = enum(c_int) {
    NONE = -1,
    FRONT_LEFT = 0,
    FRONT_RIGHT = 1,
    FRONT_CENTER = 2,
    LOW_FREQUENCY = 3,
    BACK_LEFT = 4,
    BACK_RIGHT = 5,
    FRONT_LEFT_OF_CENTER = 6,
    FRONT_RIGHT_OF_CENTER = 7,
    BACK_CENTER = 8,
    SIDE_LEFT = 9,
    SIDE_RIGHT = 10,
    TOP_CENTER = 11,
    TOP_FRONT_LEFT = 12,
    TOP_FRONT_CENTER = 13,
    TOP_FRONT_RIGHT = 14,
    TOP_BACK_LEFT = 15,
    TOP_BACK_CENTER = 16,
    TOP_BACK_RIGHT = 17,
    STEREO_LEFT = 29,
    STEREO_RIGHT = 30,
    WIDE_LEFT = 31,
    WIDE_RIGHT = 32,
    SURROUND_DIRECT_LEFT = 33,
    SURROUND_DIRECT_RIGHT = 34,
    LOW_FREQUENCY_2 = 35,
    TOP_SIDE_LEFT = 36,
    TOP_SIDE_RIGHT = 37,
    BOTTOM_FRONT_CENTER = 38,
    BOTTOM_FRONT_LEFT = 39,
    BOTTOM_FRONT_RIGHT = 40,
    UNUSED = 512,
    UNKNOWN = 768,
    AMBISONIC_BASE = 1024,
    AMBISONIC_END = 2047,
};

pub const SEEK = packed struct(c_int) {
    mode: Mode,
    padding1: u14 = 0,
    SIZE: bool,
    FORCE: bool,
    padding2: u14 = 0,

    pub const Mode = enum(u2) {
        SET = std.posix.SEEK.SET,
        CUR = std.posix.SEEK.CUR,
        END = std.posix.SEEK.END,
    };
};

pub const Codec = extern struct {
    pub const ID = enum(c_uint) {
        NONE = 0,
        MPEG1VIDEO = 1,
        MPEG2VIDEO = 2,
        H261 = 3,
        H263 = 4,
        RV10 = 5,
        RV20 = 6,
        MJPEG = 7,
        MJPEGB = 8,
        LJPEG = 9,
        SP5X = 10,
        JPEGLS = 11,
        MPEG4 = 12,
        RAWVIDEO = 13,
        MSMPEG4V1 = 14,
        MSMPEG4V2 = 15,
        MSMPEG4V3 = 16,
        WMV1 = 17,
        WMV2 = 18,
        H263P = 19,
        H263I = 20,
        FLV1 = 21,
        SVQ1 = 22,
        SVQ3 = 23,
        DVVIDEO = 24,
        HUFFYUV = 25,
        CYUV = 26,
        H264 = 27,
        INDEO3 = 28,
        VP3 = 29,
        THEORA = 30,
        ASV1 = 31,
        ASV2 = 32,
        FFV1 = 33,
        @"4XM" = 34,
        VCR1 = 35,
        CLJR = 36,
        MDEC = 37,
        ROQ = 38,
        INTERPLAY_VIDEO = 39,
        XAN_WC3 = 40,
        XAN_WC4 = 41,
        RPZA = 42,
        CINEPAK = 43,
        WS_VQA = 44,
        MSRLE = 45,
        MSVIDEO1 = 46,
        IDCIN = 47,
        @"8BPS" = 48,
        SMC = 49,
        FLIC = 50,
        TRUEMOTION1 = 51,
        VMDVIDEO = 52,
        MSZH = 53,
        ZLIB = 54,
        QTRLE = 55,
        TSCC = 56,
        ULTI = 57,
        QDRAW = 58,
        VIXL = 59,
        QPEG = 60,
        PNG = 61,
        PPM = 62,
        PBM = 63,
        PGM = 64,
        PGMYUV = 65,
        PAM = 66,
        FFVHUFF = 67,
        RV30 = 68,
        RV40 = 69,
        VC1 = 70,
        WMV3 = 71,
        LOCO = 72,
        WNV1 = 73,
        AASC = 74,
        INDEO2 = 75,
        FRAPS = 76,
        TRUEMOTION2 = 77,
        BMP = 78,
        CSCD = 79,
        MMVIDEO = 80,
        ZMBV = 81,
        AVS = 82,
        SMACKVIDEO = 83,
        NUV = 84,
        KMVC = 85,
        FLASHSV = 86,
        CAVS = 87,
        JPEG2000 = 88,
        VMNC = 89,
        VP5 = 90,
        VP6 = 91,
        VP6F = 92,
        TARGA = 93,
        DSICINVIDEO = 94,
        TIERTEXSEQVIDEO = 95,
        TIFF = 96,
        GIF = 97,
        DXA = 98,
        DNXHD = 99,
        THP = 100,
        SGI = 101,
        C93 = 102,
        BETHSOFTVID = 103,
        PTX = 104,
        TXD = 105,
        VP6A = 106,
        AMV = 107,
        VB = 108,
        PCX = 109,
        SUNRAST = 110,
        INDEO4 = 111,
        INDEO5 = 112,
        MIMIC = 113,
        RL2 = 114,
        ESCAPE124 = 115,
        DIRAC = 116,
        BFI = 117,
        CMV = 118,
        MOTIONPIXELS = 119,
        TGV = 120,
        TGQ = 121,
        TQI = 122,
        AURA = 123,
        AURA2 = 124,
        V210X = 125,
        TMV = 126,
        V210 = 127,
        DPX = 128,
        MAD = 129,
        FRWU = 130,
        FLASHSV2 = 131,
        CDGRAPHICS = 132,
        R210 = 133,
        ANM = 134,
        BINKVIDEO = 135,
        IFF_ILBM = 136,
        KGV1 = 137,
        YOP = 138,
        VP8 = 139,
        PICTOR = 140,
        ANSI = 141,
        A64_MULTI = 142,
        A64_MULTI5 = 143,
        R10K = 144,
        MXPEG = 145,
        LAGARITH = 146,
        PRORES = 147,
        JV = 148,
        DFA = 149,
        WMV3IMAGE = 150,
        VC1IMAGE = 151,
        UTVIDEO = 152,
        BMV_VIDEO = 153,
        VBLE = 154,
        DXTORY = 155,
        V410 = 156,
        XWD = 157,
        CDXL = 158,
        XBM = 159,
        ZEROCODEC = 160,
        MSS1 = 161,
        MSA1 = 162,
        TSCC2 = 163,
        MTS2 = 164,
        CLLC = 165,
        MSS2 = 166,
        VP9 = 167,
        AIC = 168,
        ESCAPE130 = 169,
        G2M = 170,
        WEBP = 171,
        HNM4_VIDEO = 172,
        HEVC = 173,
        FIC = 174,
        ALIAS_PIX = 175,
        BRENDER_PIX = 176,
        PAF_VIDEO = 177,
        EXR = 178,
        VP7 = 179,
        SANM = 180,
        SGIRLE = 181,
        MVC1 = 182,
        MVC2 = 183,
        HQX = 184,
        TDSC = 185,
        HQ_HQA = 186,
        HAP = 187,
        DDS = 188,
        DXV = 189,
        SCREENPRESSO = 190,
        RSCC = 191,
        AVS2 = 192,
        PGX = 193,
        AVS3 = 194,
        MSP2 = 195,
        VVC = 196,
        Y41P = 197,
        AVRP = 198,
        @"012V" = 199,
        AVUI = 200,
        TARGA_Y216 = 201,
        V308 = 202,
        V408 = 203,
        YUV4 = 204,
        AVRN = 205,
        CPIA = 206,
        XFACE = 207,
        SNOW = 208,
        SMVJPEG = 209,
        APNG = 210,
        DAALA = 211,
        CFHD = 212,
        TRUEMOTION2RT = 213,
        M101 = 214,
        MAGICYUV = 215,
        SHEERVIDEO = 216,
        YLC = 217,
        PSD = 218,
        PIXLET = 219,
        SPEEDHQ = 220,
        FMVC = 221,
        SCPR = 222,
        CLEARVIDEO = 223,
        XPM = 224,
        AV1 = 225,
        BITPACKED = 226,
        MSCC = 227,
        SRGC = 228,
        SVG = 229,
        GDV = 230,
        FITS = 231,
        IMM4 = 232,
        PROSUMER = 233,
        MWSC = 234,
        WCMV = 235,
        RASC = 236,
        HYMT = 237,
        ARBC = 238,
        AGM = 239,
        LSCR = 240,
        VP4 = 241,
        IMM5 = 242,
        MVDV = 243,
        MVHA = 244,
        CDTOONS = 245,
        MV30 = 246,
        NOTCHLC = 247,
        PFM = 248,
        MOBICLIP = 249,
        PHOTOCD = 250,
        IPU = 251,
        ARGO = 252,
        CRI = 253,
        SIMBIOSIS_IMX = 254,
        SGA_VIDEO = 255,
        GEM = 256,
        VBN = 257,
        JPEGXL = 258,
        QOI = 259,
        PHM = 260,
        RADIANCE_HDR = 261,
        WBMP = 262,
        MEDIA100 = 263,
        VQC = 264,
        PDV = 265,
        EVC = 266,
        RTV1 = 267,
        VMIX = 268,
        LEAD = 269,
        PCM_S16LE = 65536,
        PCM_S16BE = 65537,
        PCM_U16LE = 65538,
        PCM_U16BE = 65539,
        PCM_S8 = 65540,
        PCM_U8 = 65541,
        PCM_MULAW = 65542,
        PCM_ALAW = 65543,
        PCM_S32LE = 65544,
        PCM_S32BE = 65545,
        PCM_U32LE = 65546,
        PCM_U32BE = 65547,
        PCM_S24LE = 65548,
        PCM_S24BE = 65549,
        PCM_U24LE = 65550,
        PCM_U24BE = 65551,
        PCM_S24DAUD = 65552,
        PCM_ZORK = 65553,
        PCM_S16LE_PLANAR = 65554,
        PCM_DVD = 65555,
        PCM_F32BE = 65556,
        PCM_F32LE = 65557,
        PCM_F64BE = 65558,
        PCM_F64LE = 65559,
        PCM_BLURAY = 65560,
        PCM_LXF = 65561,
        S302M = 65562,
        PCM_S8_PLANAR = 65563,
        PCM_S24LE_PLANAR = 65564,
        PCM_S32LE_PLANAR = 65565,
        PCM_S16BE_PLANAR = 65566,
        PCM_S64LE = 65567,
        PCM_S64BE = 65568,
        PCM_F16LE = 65569,
        PCM_F24LE = 65570,
        PCM_VIDC = 65571,
        PCM_SGA = 65572,
        ADPCM_IMA_QT = 69632,
        ADPCM_IMA_WAV = 69633,
        ADPCM_IMA_DK3 = 69634,
        ADPCM_IMA_DK4 = 69635,
        ADPCM_IMA_WS = 69636,
        ADPCM_IMA_SMJPEG = 69637,
        ADPCM_MS = 69638,
        ADPCM_4XM = 69639,
        ADPCM_XA = 69640,
        ADPCM_ADX = 69641,
        ADPCM_EA = 69642,
        ADPCM_G726 = 69643,
        ADPCM_CT = 69644,
        ADPCM_SWF = 69645,
        ADPCM_YAMAHA = 69646,
        ADPCM_SBPRO_4 = 69647,
        ADPCM_SBPRO_3 = 69648,
        ADPCM_SBPRO_2 = 69649,
        ADPCM_THP = 69650,
        ADPCM_IMA_AMV = 69651,
        ADPCM_EA_R1 = 69652,
        ADPCM_EA_R3 = 69653,
        ADPCM_EA_R2 = 69654,
        ADPCM_IMA_EA_SEAD = 69655,
        ADPCM_IMA_EA_EACS = 69656,
        ADPCM_EA_XAS = 69657,
        ADPCM_EA_MAXIS_XA = 69658,
        ADPCM_IMA_ISS = 69659,
        ADPCM_G722 = 69660,
        ADPCM_IMA_APC = 69661,
        ADPCM_VIMA = 69662,
        ADPCM_AFC = 69663,
        ADPCM_IMA_OKI = 69664,
        ADPCM_DTK = 69665,
        ADPCM_IMA_RAD = 69666,
        ADPCM_G726LE = 69667,
        ADPCM_THP_LE = 69668,
        ADPCM_PSX = 69669,
        ADPCM_AICA = 69670,
        ADPCM_IMA_DAT4 = 69671,
        ADPCM_MTAF = 69672,
        ADPCM_AGM = 69673,
        ADPCM_ARGO = 69674,
        ADPCM_IMA_SSI = 69675,
        ADPCM_ZORK = 69676,
        ADPCM_IMA_APM = 69677,
        ADPCM_IMA_ALP = 69678,
        ADPCM_IMA_MTF = 69679,
        ADPCM_IMA_CUNNING = 69680,
        ADPCM_IMA_MOFLEX = 69681,
        ADPCM_IMA_ACORN = 69682,
        ADPCM_XMD = 69683,
        AMR_NB = 73728,
        AMR_WB = 73729,
        RA_144 = 77824,
        RA_288 = 77825,
        ROQ_DPCM = 81920,
        INTERPLAY_DPCM = 81921,
        XAN_DPCM = 81922,
        SOL_DPCM = 81923,
        SDX2_DPCM = 81924,
        GREMLIN_DPCM = 81925,
        DERF_DPCM = 81926,
        WADY_DPCM = 81927,
        CBD2_DPCM = 81928,
        MP2 = 86016,
        MP3 = 86017,
        AAC = 86018,
        AC3 = 86019,
        DTS = 86020,
        VORBIS = 86021,
        DVAUDIO = 86022,
        WMAV1 = 86023,
        WMAV2 = 86024,
        MACE3 = 86025,
        MACE6 = 86026,
        VMDAUDIO = 86027,
        FLAC = 86028,
        MP3ADU = 86029,
        MP3ON4 = 86030,
        SHORTEN = 86031,
        ALAC = 86032,
        WESTWOOD_SND1 = 86033,
        GSM = 86034,
        QDM2 = 86035,
        COOK = 86036,
        TRUESPEECH = 86037,
        TTA = 86038,
        SMACKAUDIO = 86039,
        QCELP = 86040,
        WAVPACK = 86041,
        DSICINAUDIO = 86042,
        IMC = 86043,
        MUSEPACK7 = 86044,
        MLP = 86045,
        GSM_MS = 86046,
        ATRAC3 = 86047,
        APE = 86048,
        NELLYMOSER = 86049,
        MUSEPACK8 = 86050,
        SPEEX = 86051,
        WMAVOICE = 86052,
        WMAPRO = 86053,
        WMALOSSLESS = 86054,
        ATRAC3P = 86055,
        EAC3 = 86056,
        SIPR = 86057,
        MP1 = 86058,
        TWINVQ = 86059,
        TRUEHD = 86060,
        MP4ALS = 86061,
        ATRAC1 = 86062,
        BINKAUDIO_RDFT = 86063,
        BINKAUDIO_DCT = 86064,
        AAC_LATM = 86065,
        QDMC = 86066,
        CELT = 86067,
        G723_1 = 86068,
        G729 = 86069,
        @"8SVX_EXP" = 86070,
        @"8SVX_FIB" = 86071,
        BMV_AUDIO = 86072,
        RALF = 86073,
        IAC = 86074,
        ILBC = 86075,
        OPUS = 86076,
        COMFORT_NOISE = 86077,
        TAK = 86078,
        METASOUND = 86079,
        PAF_AUDIO = 86080,
        ON2AVC = 86081,
        DSS_SP = 86082,
        CODEC2 = 86083,
        FFWAVESYNTH = 86084,
        SONIC = 86085,
        SONIC_LS = 86086,
        EVRC = 86087,
        SMV = 86088,
        DSD_LSBF = 86089,
        DSD_MSBF = 86090,
        DSD_LSBF_PLANAR = 86091,
        DSD_MSBF_PLANAR = 86092,
        @"4GV" = 86093,
        INTERPLAY_ACM = 86094,
        XMA1 = 86095,
        XMA2 = 86096,
        DST = 86097,
        ATRAC3AL = 86098,
        ATRAC3PAL = 86099,
        DOLBY_E = 86100,
        APTX = 86101,
        APTX_HD = 86102,
        SBC = 86103,
        ATRAC9 = 86104,
        HCOM = 86105,
        ACELP_KELVIN = 86106,
        MPEGH_3D_AUDIO = 86107,
        SIREN = 86108,
        HCA = 86109,
        FASTAUDIO = 86110,
        MSNSIREN = 86111,
        DFPWM = 86112,
        BONK = 86113,
        MISC4 = 86114,
        APAC = 86115,
        FTR = 86116,
        WAVARC = 86117,
        RKA = 86118,
        AC4 = 86119,
        OSQ = 86120,
        QOA = 86121,
        DVD_SUBTITLE = 94208,
        DVB_SUBTITLE = 94209,
        TEXT = 94210,
        XSUB = 94211,
        SSA = 94212,
        MOV_TEXT = 94213,
        HDMV_PGS_SUBTITLE = 94214,
        DVB_TELETEXT = 94215,
        SRT = 94216,
        MICRODVD = 94217,
        EIA_608 = 94218,
        JACOSUB = 94219,
        SAMI = 94220,
        REALTEXT = 94221,
        STL = 94222,
        SUBVIEWER1 = 94223,
        SUBVIEWER = 94224,
        SUBRIP = 94225,
        WEBVTT = 94226,
        MPL2 = 94227,
        VPLAYER = 94228,
        PJS = 94229,
        ASS = 94230,
        HDMV_TEXT_SUBTITLE = 94231,
        TTML = 94232,
        ARIB_CAPTION = 94233,
        TTF = 98304,
        SCTE_35 = 98305,
        EPG = 98306,
        BINTEXT = 98307,
        XBIN = 98308,
        IDF = 98309,
        OTF = 98310,
        SMPTE_KLV = 98311,
        DVD_NAV = 98312,
        TIMED_ID3 = 98313,
        BIN_DATA = 98314,
        SMPTE_2038 = 98315,
        PROBE = 102400,
        MPEG2TS = 131072,
        MPEG4SYSTEMS = 131073,
        FFMETADATA = 135168,
        WRAPPED_AVFRAME = 135169,
        VNULL = 135170,
        ANULL = 135171,

        pub const FIRST_AUDIO: ID = .PCM_S16LE;
        pub const FIRST_SUBTITLE: ID = .DVD_SUBTITLE;
        pub const FIRST_UNKNOWN: ID = .TTF;
    };

    pub const Descriptor = extern struct {
        id: ID,
        type: MediaType,
        name: [*:0]const u8,
        long_name: ?[*:0]const u8,
        props: c_int,
        mime_types: ?[*:null]const ?[*:0]const u8,
        profiles: ?[*]const Profile,
    };

    pub const Parameters = extern struct {
        codec_type: MediaType,
        codec_id: ID,
        codec_tag: u32,
        extradata: [*]u8,
        extradata_size: c_int,
        coded_side_data: [*]PacketSideData,
        nb_coded_side_data: c_int,
        format: c_int,
        bit_rate: i64,
        bits_per_coded_sample: c_int,
        bits_per_raw_sample: c_int,
        profile: c_int,
        level: c_int,
        width: c_int,
        height: c_int,
        sample_aspect_ratio: Rational,
        framerate: Rational,
        field_order: FieldOrder,
        color_range: ColorRange,
        color_primaries: ColorPrimaries,
        color_trc: ColorTransferCharacteristic,
        color_space: ColorSpace,
        chroma_location: ChromaLocation,
        video_delay: c_int,
        ch_layout: ChannelLayout,
        sample_rate: c_int,
        block_align: c_int,
        frame_size: c_int,
        initial_padding: c_int,
        trailing_padding: c_int,
        seek_preroll: c_int,
    };

    pub const Context = extern struct {
        av_class: *const Class,
        log_level_offset: c_int,
        codec_type: MediaType,
        codec: ?*const Codec,
        codec_id: ID,
        codec_tag: c_uint,
        priv_data: ?*anyopaque,
        internal: ?*opaque {},
        @"opaque": ?*anyopaque,
        bit_rate: i64,
        flags: c_int,
        flags2: c_int,
        extradata: [*]u8,
        extradata_size: c_int,
        time_base: Rational,
        pkt_timebase: Rational,
        framerate: Rational,
        ticks_per_frame: c_int,
        delay: c_int,
        width: c_int,
        height: c_int,
        coded_width: c_int,
        coded_height: c_int,
        sample_aspect_ratio: Rational,
        pix_fmt: PixelFormat,
        sw_pix_fmt: PixelFormat,
        color_primaries: ColorPrimaries,
        color_trc: ColorTransferCharacteristic,
        colorspace: ColorSpace,
        color_range: ColorRange,
        chroma_sample_location: ChromaLocation,
        field_order: FieldOrder,
        refs: c_int,
        has_b_frames: c_int,
        slice_flags: c_int,
        draw_horiz_band: ?*const fn (s: *Context, src: *const Frame, offset: *[Frame.NUM_DATA_POINTERS]c_int, y: c_int, @"type": c_int, height: c_int) callconv(.c) void,
        get_format: *const fn (s: *Context, fmt: *const PixelFormat) callconv(.c) PixelFormat,
        max_b_frames: c_int,
        b_quant_factor: f32,
        b_quant_offset: f32,
        i_quant_factor: f32,
        i_quant_offset: f32,
        lumi_masking: f32,
        temporal_cplx_masking: f32,
        spatial_cplx_masking: f32,
        p_masking: f32,
        dark_masking: f32,
        nsse_weight: c_int,
        me_cmp: c_int,
        me_sub_cmp: c_int,
        mb_cmp: c_int,
        ildct_cmp: c_int,
        dia_size: c_int,
        last_predictor_count: c_int,
        me_pre_cmp: c_int,
        pre_dia_size: c_int,
        me_subpel_quality: c_int,
        me_range: c_int,
        mb_decision: c_int,
        intra_matrix: [*]u16,
        inter_matrix: [*]u16,
        chroma_intra_matrix: [*]u16,
        intra_dc_precision: c_int,
        mb_lmin: c_int,
        mb_lmax: c_int,
        bidir_refine: c_int,
        keyint_min: c_int,
        gop_size: c_int,
        mv0_threshold: c_int,
        slices: c_int,
        sample_rate: c_int,
        sample_fmt: SampleFormat,
        ch_layout: ChannelLayout,
        frame_size: c_int,
        block_align: c_int,
        cutoff: c_int,
        audio_service_type: AudioServiceType,
        request_sample_fmt: SampleFormat,
        initial_padding: c_int,
        trailing_padding: c_int,
        seek_preroll: c_int,
        get_buffer2: *const fn (s: *Context, frame: *Frame, flags: c_int) callconv(.c) c_int,
        bit_rate_tolerance: c_int,
        global_quality: c_int,
        compression_level: c_int,
        qcompress: f32,
        qblur: f32,
        qmin: c_int,
        qmax: c_int,
        max_qdiff: c_int,
        rc_buffer_size: c_int,
        rc_override_count: c_int,
        rc_override: [*]RcOverride,
        rc_max_rate: i64,
        rc_min_rate: i64,
        rc_max_available_vbv_use: f32,
        rc_min_vbv_overflow_use: f32,
        rc_initial_buffer_occupancy: c_int,
        trellis: c_int,
        stats_out: [*]u8,
        stats_in: [*]u8,
        workaround_bugs: c_int,
        strict_std_compliance: c_int,
        error_concealment: c_int,
        debug: c_int,
        err_recognition: c_int,
        hwaccel: ?*const HWAccel,
        hwaccel_context: ?*anyopaque,
        hw_frames_ctx: ?*BufferRef,
        hw_device_ctx: ?*BufferRef,
        hwaccel_flags: c_int,
        extra_hw_frames: c_int,
        @"error": [Frame.NUM_DATA_POINTERS]u64,
        dct_algo: c_int,
        idct_algo: c_int,
        bits_per_coded_sample: c_int,
        bits_per_raw_sample: c_int,
        thread_count: c_int,
        thread_type: c_int,
        active_thread_type: c_int,
        execute: *const fn (c: *Context, *const fn (c2: *Context, arg: [*]u8) callconv(.c) c_int, arg2: [*]u8, ret: ?[*]c_int, count: c_int, size: c_int) callconv(.c) c_int,
        execute2: *const fn (c: *Context, *const fn (c2: *Context, arg: [*]u8, jobnr: c_int, threadnr: c_int) callconv(.c) c_int, arg2: [*]u8, ret: ?[*]c_int, count: c_int) callconv(.c) c_int,
        profile: c_int,
        level: c_int,
        properties: c_uint,
        skip_loop_filter: Discard,
        skip_idct: Discard,
        skip_frame: Discard,
        skip_alpha: c_int,
        skip_top: c_int,
        skip_bottom: c_int,
        lowres: c_int,
        codec_descriptor: ?*const Descriptor,
        sub_charenc: ?[*:0]u8,
        sub_charenc_mode: c_int,
        subtitle_header_size: c_int,
        subtitle_header: ?[*]u8,
        dump_separator: ?[*:0]u8,
        codec_whitelist: ?[*:0]u8,
        coded_side_data: ?[*]PacketSideData,
        nb_coded_side_data: c_int,
        export_side_data: c_int,
        max_pixels: i64,
        apply_cropping: c_int,
        discard_damaged_percentage: c_int,
        max_samples: i64,
        get_encode_buffer: *const fn (s: *Context, pkt: *Packet, flags: c_int) callconv(.c) c_int,
        frame_num: i64,
        side_data_prefer_packet: ?[*]c_int,
        nb_side_data_prefer_packet: c_uint,
        decoded_side_data: ?[*]*FrameSideData,
        nb_decoded_side_data: c_int,

        pub fn alloc(codec: *const Codec) error{OutOfMemory}!*Context {
            return avcodec_alloc_context3(codec) orelse return error.OutOfMemory;
        }

        pub fn free(self: *@This()) void {
            var keep_your_dirty_hands_off_my_pointers_ffmpeg: ?*@This() = self;
            avcodec_free_context(&keep_your_dirty_hands_off_my_pointers_ffmpeg);
        }

        /// Fill the codec context based on the values from the supplied codec
        /// parameters.
        ///
        /// Any allocated fields in codec that have a corresponding field in par
        /// are freed and replaced with duplicates of the corresponding field in
        /// par. Fields in codec that do not have a counterpart in par are not
        /// touched.
        pub fn parameters_to_context(codec: *Context, par: *const Codec.Parameters) Error!void {
            _ = try wrap(avcodec_parameters_to_context(codec, par));
        }

        pub fn open(cc: *Context, codec: *const Codec, options: ?*Dictionary.Mutable) Error!void {
            _ = try wrap(avcodec_open2(cc, codec, options));
        }

        /// Supply raw packet data as input to a decoder.
        ///
        /// Internally, this call will copy relevant `CodecContext` fields, which
        /// can influence decoding per-packet, and apply them when the packet is
        /// actually decoded. (For example `CodecContext.skip_frame`, which might
        /// direct the decoder to drop the frame contained by the packet sent with
        /// this function.)
        ///
        /// Warning: The input buffer, avpkt->data must be
        /// AV_INPUT_BUFFER_PADDING_SIZE larger than the actual read bytes because
        /// some optimized bitstream readers read 32 or 64 bits at once and could
        /// read over the end.
        ///
        /// The `CodecContext` MUST have been opened with `open` before packets may
        /// be fed to the decoder.
        ///
        /// Notable error codes:
        /// * `Error.WouldBlock`
        /// * `Error.EndOfFile`
        /// * others are also possible
        pub fn send_packet(
            cc: *Context,
            /// The input `Packet`.
            ///
            /// Usually, this will be a single video frame, or several complete
            /// audio frames.
            ///
            /// Ownership of the packet remains with the caller, and the decoder
            /// will not write to the packet.
            ///
            /// The decoder may create a reference to the packet data (or copy it
            /// if the packet is not reference-counted).
            ///
            /// Unlike with older APIs, the packet is always fully consumed, and if
            /// it contains multiple frames (e.g. some audio codecs), will require
            /// you to call `CodecContext.receive_frame` multiple times afterwards
            /// before you can send a new packet. It can be NULL (or an AVPacket
            /// with data set to NULL and size set to 0); in this case, it is
            /// considered a flush packet, which signals the end of the stream.
            /// Sending the first flush packet will return success. Subsequent ones
            /// are unnecessary and will return AVERROR_EOF. If the decoder still
            /// has frames buffered, it will return them after sending a flush
            /// packet.
            packet: ?*const Packet,
        ) Error!void {
            _ = try wrap(avcodec_send_packet(cc, packet));
        }

        /// Return decoded output data from a decoder or encoder (when the
        /// AV_CODEC_FLAG_RECON_FRAME flag is used).
        ///
        /// Notable error codes:
        /// * `Error.WouldBlock`
        /// * `Error.EndOfFile`
        /// * others are also possible
        pub fn receive_frame(
            avctx: *Context,
            /// This will be set to a reference-counted video or audio frame
            /// (depending on the decoder type) allocated by the codec. Note that
            /// the function will always call `Frame.unref` before doing anything
            /// else.
            frame: *Frame,
        ) Error!void {
            _ = try wrap(avcodec_receive_frame(avctx, frame));
        }

        /// Reset the internal codec state / flush internal buffers. Should be called
        /// e.g. when seeking or when switching to a different stream.
        ///
        /// For decoders, this function just releases any references the decoder
        /// might keep internally, but the caller's references remain valid.
        ///
        /// For encoders, this function will only do something if the encoder
        /// declares support for AV_CODEC_CAP_ENCODER_FLUSH. When called, the encoder
        /// will drain any remaining packets, and can then be re-used for a different
        /// stream (as opposed to sending a null frame which will leave the encoder
        /// in a permanent EOF state after draining). This can be desirable if the
        /// cost of tearing down and replacing the encoder instance is high.
        ///
        pub const flush_buffers = avcodec_flush_buffers;
    };

    name: [*:0]const u8,
    long_name: ?[*:0]const u8,
    type: MediaType,
    id: ID,
    capabilities: c_int,
    max_lowres: u8,
    supported_framerates: ?[*]const Rational,
    pix_fmts: ?[*:.NONE]const PixelFormat,
    supported_samplerates: ?[*:0]const c_int,
    sample_fmts: ?[*:.NONE]const SampleFormat,
    priv_class: *const Class,
    profiles: ?[*]const Profile,
    wrapper_name: ?[*:0]const u8,
    ch_layouts: [*]const ChannelLayout,

    /// Iterate over all registered codecs.
    pub const iterate = av_codec_iterate;

    /// Find a registered decoder with a matching codec ID.
    pub fn find_decoder(id: ID) error{DecoderNotFound}!*const Codec {
        return avcodec_find_decoder(id) orelse error.DecoderNotFound;
    }

    /// Find a registered decoder with the specified name.
    pub fn find_decoder_by_name(name: [*:0]const u8) error{DecoderNotFound}!*const Codec {
        return avcodec_find_decoder_by_name(name) orelse error.DecoderNotFound;
    }

    /// Find a registered encoder with a matching codec ID.
    pub fn find_encoder(id: ID) error{EncoderNotFound}!*const Codec {
        return avcodec_find_encoder(id) orelse error.EncoderNotFound;
    }

    /// Find a registered encoder with the specified name.
    pub fn find_encoder_by_name(name: [*:0]const u8) error{EncoderNotFound}!*const Codec {
        return avcodec_find_encoder_by_name(name) orelse error.EncoderNotFound;
    }

    pub fn is_encoder(codec: *const Codec) bool {
        return av_codec_is_encoder(codec) != 0;
    }

    pub fn is_decoder(codec: *const Codec) bool {
        return av_codec_is_decoder(codec) != 0;
    }

    /// Return a name for the specified profile, if available.
    pub fn get_profile_name(codec: *const Codec, profile: c_int) error{ProfileNotFound}![*:0]const u8 {
        return av_get_profile_name(codec, profile) orelse error.ProfileNotFound;
    }
};

/// Decoded (raw) audio or video data.
///
/// `Frame` must be allocated using `Frame.alloc`. Note that this only
/// allocates the `Frame` itself, the buffers for the data must be managed
/// through other means (see below).
///
/// `Frame` must be freed with `Frame.free`.
///
/// `Frame` is typically allocated once and then reused multiple times to hold
/// different data (e.g. a single `Frame` to hold frames received from a
/// decoder). In such a case, `Frame.unref` will free any references held by
/// the frame and reset it to its original clean state before it is reused
/// again.
///
/// The data described by an `Frame` is usually reference counted through the
/// AVBuffer API. The underlying buffer references are stored in `Frame`.buf /
/// `Frame`.extended_buf. An `Frame` is considered to be reference counted if
/// at least one reference is set, i.e. if `Frame`.buf[0] != NULL. In such a
/// case, every single data plane must be contained in one of the buffers in
/// `Frame`.buf or `Frame`.extended_buf.
///
/// There may be a single buffer for all the data, or one separate buffer for
/// each plane, or anything in between.
///
/// `@sizeOf(Frame)` is not a part of the public ABI, so new fields may be
/// added to the end with a minor bump.
///
/// Fields can be accessed through `Options`, the name string used, matches the
/// C structure field name for fields accessible through `Options`. The `Class`
/// for `Frame` can be obtained from avcodec_get_frame_class()
pub const Frame = extern struct {
    pub const NUM_DATA_POINTERS = 8;

    data: [NUM_DATA_POINTERS][*]u8,
    linesize: [NUM_DATA_POINTERS]c_int,
    /// Pointers to the data planes/channels.
    ///
    /// For video, this should simply point to data[].
    ///
    /// For planar audio, each channel has a separate data pointer, and
    /// linesize[0] contains the size of each channel buffer.
    /// For packed audio, there is just one data pointer, and linesize[0]
    /// contains the total size of the buffer for all channels.
    ///
    /// Note: Both data and extended_data should always be set in a valid frame,
    /// but for planar audio with more channels that can fit in data,
    /// extended_data must be used in order to access all channels.
    extended_data: [*][*]u8,
    width: c_int,
    height: c_int,
    /// Number of audio samples (per channel) described by this frame.
    nb_samples: c_int,
    format: extern union {
        pixel: PixelFormat,
        sample: SampleFormat,
    },
    key_frame: c_int,
    pict_type: PictureType,
    sample_aspect_ratio: Rational,
    pts: i64,
    pkt_dts: i64,
    /// Time base for the timestamps in this frame.
    ///
    /// In the future, this field may be set on frames output by decoders or
    /// filters, but its value will be by default ignored on input to encoders
    /// or filters.
    time_base: Rational,
    quality: c_int,
    @"opaque": ?*anyopaque,
    repeat_pict: c_int,
    interlaced_frame: c_int,
    top_field_first: c_int,
    palette_has_changed: c_int,
    /// Sample rate of the audio data.
    sample_rate: c_int,
    buf: [NUM_DATA_POINTERS]*BufferRef,
    extended_buf: [*]*BufferRef,
    nb_extended_buf: c_int,
    side_data: [*]*FrameSideData,
    nb_side_data: c_int,
    flags: c_int,
    color_range: ColorRange,
    color_primaries: ColorPrimaries,
    color_trc: ColorTransferCharacteristic,
    colorspace: ColorSpace,
    chroma_location: ChromaLocation,
    best_effort_timestamp: i64,
    pkt_pos: i64,
    metadata: Dictionary.Mutable,
    decode_error_flags: c_int,
    pkt_size: c_int,
    hw_frames_ctx: *BufferRef,
    opaque_ref: *BufferRef,
    crop_top: usize,
    crop_bottom: usize,
    crop_left: usize,
    crop_right: usize,
    private_ref: *BufferRef,
    /// Channel layout of the audio data.
    ch_layout: ChannelLayout,
    duration: i64,

    /// Allocate a `Frame` and set its fields to default values.  The resulting
    /// struct must be freed using `free`.
    ///
    /// Returns a `Frame` filled with default values.
    ///
    /// This only allocates the `Frame` itself, not the data buffers. Those
    /// must be allocated through other means, e.g. with av_frame_get_buffer()
    /// or manually.
    pub fn alloc() error{OutOfMemory}!*Frame {
        return av_frame_alloc() orelse error.OutOfMemory;
    }

    /// Free the frame and any dynamically allocated objects in it, e.g.
    /// extended_data. If the frame is reference counted, it will be
    /// unreferenced first.
    pub fn free(frame: *Frame) void {
        var keep_your_dirty_hands_off_my_pointers_ffmpeg: ?*Frame = frame;
        av_frame_free(&keep_your_dirty_hands_off_my_pointers_ffmpeg);
    }

    /// Set up a new reference to the data described by the source frame.
    ///
    /// Copy frame properties from src to dst and create a new reference for
    /// each `BufferRef` from src.
    ///
    /// If src is not reference counted, new buffers are allocated and the data
    /// is copied.
    ///
    /// Warning: dst MUST have been either unreferenced with `unref`, or newly
    /// allocated with `alloc` before calling this function, or undefined
    /// behavior will occur.
    pub fn ref(dst: *Frame, src: *const Frame) error{OutOfMemory}!void {
        _ = wrap(av_frame_ref(dst, src)) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => unreachable, // I checked the source code, those are the only possible errors.
        };
    }

    /// Unreference all the buffers referenced by frame and reset the frame fields.
    pub const unref = av_frame_unref;
};

pub const PictureType = enum(c_uint) {
    NONE = 0,
    I = 1,
    P = 2,
    B = 3,
    S = 4,
    SI = 5,
    SP = 6,
    BI = 7,
};

pub const FrameSideData = extern struct {
    type: FrameSideDataType,
    data: [*c]u8,
    size: usize,
    metadata: Dictionary.Mutable,
    buf: [*c]BufferRef,
};

pub const FrameSideDataType = enum(c_uint) {
    PANSCAN = 0,
    A53_CC = 1,
    STEREO3D = 2,
    MATRIXENCODING = 3,
    DOWNMIX_INFO = 4,
    REPLAYGAIN = 5,
    DISPLAYMATRIX = 6,
    AFD = 7,
    MOTION_VECTORS = 8,
    SKIP_SAMPLES = 9,
    AUDIO_SERVICE_TYPE = 10,
    MASTERING_DISPLAY_METADATA = 11,
    GOP_TIMECODE = 12,
    SPHERICAL = 13,
    CONTENT_LIGHT_LEVEL = 14,
    ICC_PROFILE = 15,
    S12M_TIMECODE = 16,
    DYNAMIC_HDR_PLUS = 17,
    REGIONS_OF_INTEREST = 18,
    VIDEO_ENC_PARAMS = 19,
    SEI_UNREGISTERED = 20,
    FILM_GRAIN_PARAMS = 21,
    DETECTION_BBOXES = 22,
    DOVI_RPU_BUFFER = 23,
    DOVI_METADATA = 24,
    DYNAMIC_HDR_VIVID = 25,
    AMBIENT_VIEWING_ENVIRONMENT = 26,
    VIDEO_HINT = 27,
};

pub const AudioServiceType = enum(c_uint) {
    MAIN = 0,
    EFFECTS = 1,
    VISUALLY_IMPAIRED = 2,
    HEARING_IMPAIRED = 3,
    DIALOGUE = 4,
    COMMENTARY = 5,
    EMERGENCY = 6,
    VOICE_OVER = 7,
    KARAOKE = 8,
};

pub const RcOverride = extern struct {
    start_frame: c_int,
    end_frame: c_int,
    qscale: c_int,
    quality_factor: f32,
};

pub const HWAccel = extern struct {
    name: [*c]const u8,
    type: MediaType,
    id: Codec.ID,
    pix_fmt: PixelFormat,
    capabilities: c_int,
};

pub const FilterGraph = extern struct {
    av_class: *const Class,
    filters: [*]*FilterContext,
    nb_filters: c_uint,
    scale_sws_opts: [*:0]u8,
    thread_type: c_int,
    nb_threads: c_int,
    @"opaque": ?*anyopaque,
    execute: ?*const filter_execute_func,
    aresample_swr_opts: [*:0]u8,

    pub fn alloc() error{OutOfMemory}!*FilterGraph {
        return avfilter_graph_alloc() orelse return error.OutOfMemory;
    }

    pub fn free(fg: *FilterGraph) void {
        var keep_your_dirty_hands_off_my_pointers_ffmpeg: ?*FilterGraph = fg;
        avfilter_graph_free(&keep_your_dirty_hands_off_my_pointers_ffmpeg);
    }

    /// Create a new filter instance in a filter graph.
    ///
    /// Returns the context of the newly created filter instance.
    ///
    /// The filter instance is also retrievable directly through
    /// `FilterGraph.filters` or with `FilterGraph.get_filter`.
    pub fn alloc_filter(
        graph: *FilterGraph,
        /// The filter to create an instance of.
        filter: *const Filter,
        /// Name to give to the new instance (will be copied to
        /// `FilterContext.name`). This may be used by the caller to identify
        /// different filters, libavfilter itself assigns no semantics to this
        /// parameter. May be `null`.
        name: ?[*:0]const u8,
    ) error{OutOfMemory}!*FilterContext {
        return avfilter_graph_alloc_filter(graph, filter, name) orelse return error.OutOfMemory;
    }

    /// Check validity and configure all the links and formats in the graph.
    pub fn config(graph: *FilterGraph, log_ctx: ?*anyopaque) Error!void {
        _ = try wrap(avfilter_graph_config(graph, log_ctx));
    }
};

/// An instance of a filter.
pub const FilterContext = extern struct {
    av_class: *const Class,
    filter: *const Filter,
    name: ?[*:0]u8,
    input_pads: ?*FilterPad,
    inputs: [*]*FilterLink,
    nb_inputs: c_uint,
    output_pads: ?*FilterPad,
    outputs: [*]*FilterLink,
    nb_outputs: c_uint,
    priv: ?*anyopaque,
    graph: *FilterGraph,
    thread_type: c_int,
    nb_threads: c_int,
    command_queue: ?*opaque {},
    enable_str: [*:0]u8,
    enable: ?*anyopaque,
    var_values: [*]f64,
    is_disabled: c_int,
    hw_device_ctx: *BufferRef,
    ready: c_uint,
    extra_hw_frames: c_int,

    /// Sets the filter context parameter with the given name to value.
    ///
    /// SI postfixes and some named scalars are supported.
    ///
    /// If the field is of a numeric type, it has to be a numeric or named
    /// scalar. Behavior with more than one scalar and +- infix operators is
    /// undefined.
    ///
    /// If the field is of a flags type, it has to be a sequence of numeric
    /// scalars or named flags separated by '+' or '-'. Prefixing a flag with
    /// '+' causes it to be set without affecting the other flags; similarly,
    /// '-' unsets a flag.
    ///
    /// If the field is of a dictionary type, it has to be a ':' separated list
    /// of key=value parameters. Values containing ':' special characters must
    /// be escaped.
    ///
    /// Asserts:
    /// * a matching named option exists
    /// * the value is valid and in range
    pub fn opt_set(
        fc: *FilterContext,
        name: [*:0]const u8,
        /// If the field being set is not of a string type, then the given
        /// string is parsed.
        val: [*:0]const u8,
    ) void {
        _ = wrap(av_opt_set(fc, name, val, .{ .CHILDREN = true })) catch unreachable;
    }

    /// Sets the filter context parameter with the given name to an integer value.
    ///
    /// Asserts:
    /// * a matching named option exists
    /// * the value is valid and in range
    pub fn opt_set_int(fc: *FilterContext, name: [*:0]const u8, val: i64) void {
        _ = wrap(av_opt_set_int(fc, name, val, .{ .CHILDREN = true })) catch unreachable;
    }

    /// Sets the filter context parameter with the given name to a 64-bit float value.
    ///
    /// Asserts:
    /// * a matching named option exists
    /// * the value is valid and in range
    pub fn opt_set_double(fc: *FilterContext, name: [*:0]const u8, val: f64) void {
        _ = wrap(av_opt_set_double(fc, name, val, .{ .CHILDREN = true })) catch unreachable;
    }

    /// Sets the filter context parameter with the given name to a `Rational` value.
    ///
    /// Asserts:
    /// * a matching named option exists
    /// * the value is valid and in range
    pub fn opt_set_q(fc: *FilterContext, name: [*:0]const u8, val: Rational) void {
        _ = wrap(av_opt_set_q(fc, name, val, .{ .CHILDREN = true })) catch unreachable;
    }

    /// Get a value of the option with the given name.
    pub fn opt_get_double(fc: *FilterContext, option_name: [*:0]const u8) Error!f64 {
        var result: f64 = undefined;
        _ = try wrap(av_opt_get_double(fc, option_name, .{ .CHILDREN = true }, &result));
        return result;
    }

    /// Initialize a filter with the supplied parameters.
    pub fn init_str(
        /// Uninitialized filter context to initialize.
        ctx: *FilterContext,
        /// Options to initialize the filter with. This must be a ':'-separated
        /// list of options in the 'key=value' form.
        ///
        /// May be NULL if the options have been set directly using the
        /// `Options` API or there are no options that need to be set.
        args: ?[*:0]const u8,
    ) Error!void {
        _ = try wrap(avfilter_init_str(ctx, args));
    }

    /// Link two filters together.
    ///
    /// @param src    the source filter
    /// @param srcpad index of the output pad on the source filter
    /// @param dst    the destination filter
    /// @param dstpad index of the input pad on the destination filter
    /// @return       zero on success
    pub fn link(
        src: *FilterContext,
        src_pad: c_uint,
        dst: *FilterContext,
        dst_pad: c_uint,
    ) error{OutOfMemory}!void {
        _ = wrap(avfilter_link(src, src_pad, dst, dst_pad)) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => unreachable, // I checked the source code, those are the only possible errors.
        };
    }

    /// Add a frame to the buffer source.
    ///
    /// This function is equivalent to `buffersrc_add_frame_flags` with the
    /// AV_BUFFERSRC_FLAG_KEEP_REF flag.
    pub fn buffersrc_write_frame(
        /// An instance of the buffersrc filter.
        ctx: *FilterContext,
        /// Frame to be added.
        ///
        /// If the frame is reference counted, this function will make a new
        /// reference to it. Otherwise the frame data will be copied.
        frame: *const Frame,
    ) Error!void {
        _ = try wrap(av_buffersrc_write_frame(ctx, frame));
    }

    /// Add a frame to the buffer source.
    ///
    /// The difference between this function and `buffersrc_write_frame` is
    /// that `buffersrc_write_frame` creates a new reference to the input
    /// frame, while this function takes ownership of the reference passed to it.
    ///
    /// This function is equivalent to `buffersrc_add_frame_flags` without the
    /// AV_BUFFERSRC_FLAG_KEEP_REF flag.
    pub fn buffersrc_add_frame(
        /// An instance of the buffersrc filter.
        ctx: *FilterContext,
        /// Frame to be added.
        ///
        /// If the frame is reference counted, this function will take
        /// ownership of the reference(s) and reset the frame. Otherwise the
        /// frame data will be copied. If this function returns an error, the
        /// input frame is not touched.
        ///
        /// `null` indicates a flush and will cause EOF to come out the other
        /// end of the filter graph.
        frame: ?*Frame,
    ) Error!void {
        _ = try wrap(av_buffersrc_add_frame(ctx, frame));
    }

    /// Get a frame with filtered data from sink and put it in frame.
    pub fn buffersink_get_frame_flags(
        /// Pointer to a buffersink or abuffersink filter context.
        ctx: *FilterContext,
        /// Pointer to an allocated frame that will be filled with data.
        ///
        /// The data must be freed using `Frame.unref` / `Frame.free`.
        frame: *Frame,
        flags: BUFFERSINK_FLAG,
    ) Error!void {
        _ = try wrap(av_buffersink_get_frame_flags(ctx, frame, flags));
    }

    /// Same as `buffersink_get_frame`, but with the ability to specify the
    /// number of samples read.
    ///
    /// This function is less efficient than `buffersink_get_frame`, because it
    /// copies the data around.
    ///
    /// Warning: do not mix this function with `buffersink_get_frame`. Use only
    /// one or the other with a single sink, not both.
    pub fn buffersink_get_samples(
        /// Pointer to a context of the abuffersink `Filter`.
        ctx: *FilterContext,
        /// pointer to an allocated frame that will be filled with data.
        ///
        /// The data must be freed using `Frame.unref / `Frame.free`.
        frame: *Frame,
        /// `frame` will contain exactly nb_samples audio samples, except at
        /// the end of stream, when it can contain less than nb_samples.
        nb_samples: c_int,
    ) Error!void {
        _ = try wrap(av_buffersink_get_samples(ctx, frame, nb_samples));
    }

    /// Set the frame size for an audio buffer sink.
    ///
    /// All calls to `buffersink_get_frame_flags` will return a buffer with
    /// exactly the specified number of samples, or `error.WouldBlock` if there
    /// is not enough. The last buffer at EOF will be padded with 0.
    pub const buffersink_set_frame_size = av_buffersink_set_frame_size;
};

pub const BUFFERSINK_FLAG = packed struct(c_uint) {
    /// Tell `FilterContext.buffersink_get_frame_flags` to read video/samples
    /// buffer reference, but not remove it from the buffer. This is useful if you
    /// need only to read a video/samples buffer, without to fetch it.
    PEEK: bool = false,

    /// Tell `FilterContext.buffersink_get_frame_flags` not to request a frame from
    /// its input. If a frame is already buffered, it is read (and removed from the
    /// buffer), but if no frame is present, return `Error.WouldBlock`.
    NO_REQUEST: bool = false,

    _: u30 = 0,
};

pub const filter_execute_func = fn ([*c]FilterContext, ?*const filter_action_func, ?*anyopaque, [*c]c_int, c_int) callconv(.c) c_int;
pub const filter_action_func = fn ([*c]FilterContext, ?*anyopaque, c_int, c_int) callconv(.c) c_int;
pub const FilterLink = extern struct {
    src: [*c]FilterContext,
    srcpad: ?*FilterPad,
    dst: [*c]FilterContext,
    dstpad: ?*FilterPad,
    type: MediaType,
    format: c_int,
    w: c_int,
    h: c_int,
    sample_aspect_ratio: Rational,
    colorspace: ColorSpace,
    color_range: ColorRange,
    sample_rate: c_int,
    ch_layout: ChannelLayout,
    time_base: Rational,
    incfg: FilterFormatsConfig,
    outcfg: FilterFormatsConfig,
    graph: [*c]FilterGraph,
    current_pts: i64,
    current_pts_us: i64,
    frame_rate: Rational,
    min_samples: c_int,
    max_samples: c_int,
    frame_count_in: i64,
    frame_count_out: i64,
    sample_count_in: i64,
    sample_count_out: i64,
    frame_wanted_out: c_int,
    hw_frames_ctx: [*c]BufferRef,
};

pub const Filter = extern struct {
    name: [*c]const u8,
    description: [*c]const u8,
    inputs: ?*const FilterPad,
    outputs: ?*const FilterPad,
    priv_class: [*c]const Class,
    flags: c_int,
    nb_inputs: u8,
    nb_outputs: u8,
    formats_state: u8,
    preinit: ?*const fn ([*c]FilterContext) callconv(.c) c_int,
    init: ?*const fn ([*c]FilterContext) callconv(.c) c_int,
    uninit: ?*const fn ([*c]FilterContext) callconv(.c) void,
    formats: extern union {
        query_func: ?*const fn ([*c]FilterContext) callconv(.c) c_int,
        pixels_list: [*c]const PixelFormat,
        samples_list: [*c]const SampleFormat,
        pix_fmt: PixelFormat,
        sample_fmt: SampleFormat,
    },
    priv_size: c_int,
    flags_internal: c_int,
    process_command: ?*const fn ([*c]FilterContext, [*c]const u8, [*c]const u8, [*c]u8, c_int, c_int) callconv(.c) c_int,
    activate: ?*const fn ([*c]FilterContext) callconv(.c) c_int,

    /// Get a filter definition matching the given name.
    ///
    /// Returns the filter definition, if any matching one is registered, or
    /// `null` if none found.
    pub const get_by_name = avfilter_get_by_name;
};

pub const FilterPad = opaque {};

pub const FilterFormatsConfig = extern struct {
    formats: ?*FilterFormats,
    samplerates: ?*FilterFormats,
    channel_layouts: ?*FilterChannelLayouts,
};

pub const FilterFormats = opaque {};
pub const FilterChannelLayouts = opaque {};

pub const RDFTransformType = enum(c_uint) {
    DFT_R2C = 0,
    IDFT_C2R = 1,
    IDFT_R2C = 2,
    DFT_C2R = 3,
};

pub const RDFTContext = opaque {
    pub fn init(nbits: c_int, trans: RDFTransformType) error{OutOfMemory}!*RDFTContext {
        return av_rdft_init(nbits, trans) orelse return error.OutOfMemory;
    }

    pub const calc = av_rdft_calc;
    pub const end = av_rdft_end;
};
pub const FFTSample = f32;

pub const TXType = enum(c_uint) {
    FLOAT_FFT = 0,
    DOUBLE_FFT = 2,
    INT32_FFT = 4,
    FLOAT_MDCT = 1,
    DOUBLE_MDCT = 3,
    INT32_MDCT = 5,
    /// Real to complex and complex to real DFT.
    ///
    /// The forward transform performs a real-to-complex DFT of N samples to
    /// N/2+1 complex values.
    ///
    /// The inverse transform performs a complex-to-real DFT of N/2+1 complex
    /// values to N real samples. The output is not normalized, but can be
    /// made so by setting the scale value to 1.0/len.
    ///
    /// The inverse transform always overwrites the input.
    FLOAT_RDFT = 6,
    DOUBLE_RDFT = 7,
    INT32_RDFT = 8,
    FLOAT_DCT = 9,
    DOUBLE_DCT = 10,
    INT32_DCT = 11,
    FLOAT_DCT_I = 12,
    DOUBLE_DCT_I = 13,
    INT32_DCT_I = 14,
    FLOAT_DST_I = 15,
    DOUBLE_DST_I = 16,
    INT32_DST_I = 17,

    pub fn SampleType(comptime t: TXType) type {
        return switch (t) {
            .FLOAT_FFT,
            .FLOAT_MDCT,
            .FLOAT_RDFT,
            .FLOAT_DCT,
            .FLOAT_DCT_I,
            .FLOAT_DST_I,
            => f32,

            .DOUBLE_FFT,
            .DOUBLE_MDCT,
            .DOUBLE_RDFT,
            .DOUBLE_DCT,
            .DOUBLE_DCT_I,
            .DOUBLE_DST_I,
            => f64,

            .INT32_FFT,
            .INT32_MDCT,
            .INT32_RDFT,
            .INT32_DCT,
            .INT32_DCT_I,
            .INT32_DST_I,
            => i32,
        };
    }

    pub fn ScaleType(comptime t: TXType) type {
        return switch (t) {
            .FLOAT_FFT,
            .FLOAT_MDCT,
            .FLOAT_RDFT,
            .FLOAT_DCT,
            .FLOAT_DCT_I,
            .FLOAT_DST_I,
            .INT32_FFT,
            .INT32_MDCT,
            .INT32_RDFT,
            .INT32_DCT,
            .INT32_DCT_I,
            .INT32_DST_I,
            => f32,

            .DOUBLE_FFT,
            .DOUBLE_MDCT,
            .DOUBLE_RDFT,
            .DOUBLE_DCT,
            .DOUBLE_DCT_I,
            .DOUBLE_DST_I,
            => f64,
        };
    }

    pub fn stride(t: TXType, inverse: bool) isize {
        return switch (t) {
            .FLOAT_FFT,
            .FLOAT_DCT,
            .FLOAT_DCT_I,
            .FLOAT_DST_I,
            => @sizeOf(f32),

            .DOUBLE_FFT,
            .DOUBLE_DCT,
            .DOUBLE_DCT_I,
            .DOUBLE_DST_I,
            => @sizeOf(f64),

            .INT32_FFT,
            .INT32_DCT,
            .INT32_DCT_I,
            .INT32_DST_I,
            => @sizeOf(i32),

            .FLOAT_MDCT => if (!inverse) @sizeOf(f32) else @sizeOf([2]ComplexFloat),
            .DOUBLE_MDCT => if (!inverse) @sizeOf(f64) else @sizeOf([2]ComplexFloat),
            .INT32_MDCT => if (!inverse) @sizeOf(i32) else @sizeOf([2]ComplexFloat),

            .FLOAT_RDFT => if (!inverse) @sizeOf([2]f32) else @sizeOf([2]ComplexFloat),
            .DOUBLE_RDFT => if (!inverse) @sizeOf([2]f64) else @sizeOf([2]ComplexFloat),
            .INT32_RDFT => if (!inverse) @sizeOf([2]i32) else @sizeOf([2]ComplexFloat),
        };
    }
};

pub const TXFlags = packed struct(u64) {
    INPLACE: bool = false,
    UNALIGNED: bool = false,
    FULL_IMDCT: bool = false,
    REAL_TO_REAL: bool = false,
    REAL_TO_IMAGINARY: bool = false,
    _: u59 = 0,
};

pub const TXContext = opaque {
    /// Initialize a transform context with the given configuration.
    ///
    /// (i)MDCTs with an odd length are currently not supported.
    pub fn init(
        /// type type the type of transform
        comptime tx_type: TXType,
        /// whether to do an inverse or a forward transform
        inverse: bool,
        /// len the size of the transform in samples
        len: c_int,
        /// The value to scale the output if supported by type.
        scale: tx_type.ScaleType(),
        flags: TXFlags,
    ) Error!struct {
        context: *TXContext,
        tx_fn: *const tx_fn,
        stride_in_bytes: isize,

        pub fn tx(s: @This(), output_array: [*]tx_type.SampleType(), input_array: [*]tx_type.SampleType()) void {
            s.tx_fn(s.context, output_array, input_array, s.stride_in_bytes);
        }
    } {
        var ctx: ?*TXContext = null;
        var tx: ?*const tx_fn = null;
        _ = try wrap(av_tx_init(&ctx, &tx, tx_type, @intFromBool(inverse), len, &scale, flags));
        return .{
            .context = ctx.?,
            .tx_fn = tx.?,
            .stride_in_bytes = tx_type.stride(inverse),
        };
    }

    pub fn uninit(ctx: *TXContext) void {
        var keep_your_dirty_hands_off_my_pointers_ffmpeg: ?*TXContext = ctx;
        av_tx_uninit(&keep_your_dirty_hands_off_my_pointers_ffmpeg);
    }
};

pub const ComplexFloat = extern struct {
    re: f32,
    im: f32,
};

pub const StreamGroup = extern struct {
    av_class: *const Class,
    priv_data: ?*anyopaque,
    index: c_uint,
    id: i64,
    type: ParamsType,
    params: extern union {
        iamf_audio_element: ?*IAMFAudioElement,
        iamf_mix_presentation: ?*IAMFMixPresentation,
        tile_grid: *TileGrid,
    },
    metadata: Dictionary.Mutable,
    nb_streams: c_uint,
    streams: [*]*Stream,
    disposition: c_int,

    pub const ParamsType = enum(c_uint) {
        NONE = 0,
        IAMF_AUDIO_ELEMENT = 1,
        IAMF_MIX_PRESENTATION = 2,
        TILE_GRID = 3,
    };

    pub const TileGrid = opaque {};
};

pub const IAMFAudioElement = opaque {};
pub const IAMFMixPresentation = opaque {};

pub const sws = struct {
    pub const Flags = packed struct(c_int) {
        FAST_BILINEAR: bool = false,
        BILINEAR: bool = false,
        BICUBIC: bool = false,
        X: bool = false,
        POINT: bool = false,
        AREA: bool = false,
        BICUBLIN: bool = false,
        GAUSS: bool = false,
        SINC: bool = false,
        LANCZOS: bool = false,
        SPLINE: bool = false,
        unused11: u1 = 0,
        PRINT_INFO: bool = false,
        /// not completely implemented
        /// internal chrominance subsampling info
        FULL_CHR_H_INT: bool = false,
        /// not completely implemented
        /// input subsampling info
        FULL_CHR_H_INP: bool = false,
        /// not completely implemented
        DIRECT_BGR: bool = false,
        SRC_V_CHR_DROP: u2 = 0,
        ACCURATE_RND: bool = false,
        BITEXACT: bool = false,
        unused20: u3 = 0,
        ERROR_DIFFUSION: bool = false,
        unused24: @Type(.{ .int = .{ .signedness = .unsigned, .bits = @bitSizeOf(c_int) - 24 } }) = 0,
    };

    pub const Vector = extern struct {
        coeff: [*]f64,
        length: c_int,
    };

    pub const Filter = extern struct {
        lumH: ?*Vector,
        lumV: ?*Vector,
        chrH: ?*Vector,
        chrV: ?*Vector,
    };

    pub const Context = opaque {
        /// Allocate an empty sws.Context. This must be filled and passed to
        /// init().
        pub fn alloc() error{OutOfMemory}!*Context {
            return sws_alloc_context() orelse error.OutOfMemory;
        }

        /// Initialize the swscaler context sws_context.
        pub fn init(sws_context: *Context, srcFilter: ?*sws.Filter, dstFilter: ?*sws.Filter) Error!void {
            _ = try wrap(sws_init_context(sws_context, srcFilter, dstFilter));
        }

        /// Free the swscaler context swsContext.
        pub const free = sws_freeContext;

        /// Allocate and return an sws.Context. You need it to perform
        /// scaling/conversion operations using sws.Context.scale().
        pub fn get(srcW: c_int, srcH: c_int, srcFormat: PixelFormat, dstW: c_int, dstH: c_int, dstFormat: PixelFormat, flags: Flags, srcFilter: ?*sws.Filter, dstFilter: ?*sws.Filter, param: ?[*]const f64) error{OutOfMemory}!void {
            return sws_getContext(srcW, srcH, srcFormat, dstW, dstH, dstFormat, flags, srcFilter, dstFilter, param) orelse error.OutOfMemory;
        }

        /// Scale the image slice in srcSlice and put the resulting scaled
        /// slice in the image in dst. A slice is a sequence of consecutive
        /// rows in an image.
        ///
        /// Slices have to be provided in sequential order.
        pub fn scale(c: *Context, srcSlice: [*]const [*]const u8, srcStride: [*]const c_int, srcSliceY: c_int, srcSliceH: c_int, dst: [*]const [*]u8, dstStride: [*]const c_int) Error!void {
            _ = try wrap(sws_scale(c, srcSlice, srcStride, srcSliceY, srcSliceH, dst, dstStride));
        }

        /// Scale source data from src and write the output to dst.
        pub fn scale_frame(c: *Context, dst: *Frame, src: *const Frame) Error!void {
            _ = try wrap(sws_scale_frame(c, dst, src));
        }
    };
};
