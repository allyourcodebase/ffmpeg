// If you're updating ffmpeg you must preserve the following include, and the
// #ifs scattered throughout the file
#include "config_components.h"

static const URLProtocol * const url_protocols[] = {
    &ff_async_protocol,
    &ff_cache_protocol,
    &ff_concat_protocol,
    &ff_concatf_protocol,
    &ff_crypto_protocol,
    &ff_data_protocol,
    &ff_fd_protocol,
#if CONFIG_FFRTMPHTTP_PROTOCOL
    &ff_ffrtmphttp_protocol,
#endif
    &ff_file_protocol,
#if CONFIG_FTP_PROTOCOL
    &ff_ftp_protocol,
#endif
#if CONFIG_GOPHER_PROTOCOL
    &ff_gopher_protocol,
#endif
#if CONFIG_HTTP_PROTOCOL
    &ff_http_protocol,
#endif
#if CONFIG_HTTPPROXY_PROTOCOL
    &ff_httpproxy_protocol,
#endif
#if CONFIG_ICECAST_PROTOCOL
    &ff_icecast_protocol,
#endif
#if CONFIG_MMSH_PROTOCOL
    &ff_mmsh_protocol,
#endif
#if CONFIG_MMST_PROTOCOL
    &ff_mmst_protocol,
#endif
    &ff_md5_protocol,
    &ff_pipe_protocol,
    &ff_prompeg_protocol,
#if CONFIG_RTMP_PROTOCOL
    &ff_rtmp_protocol,
#endif
#if CONFIG_RTMPT_PROTOCOL
    &ff_rtmpt_protocol,
#endif
#if CONFIG_RTP_PROTOCOL
    &ff_rtp_protocol,
#endif
    &ff_srtp_protocol,
    &ff_subfile_protocol,
    &ff_tee_protocol,
#if CONFIG_TCP_PROTOCOL
    &ff_tcp_protocol,
#endif
#if CONFIG_UDP_PROTOCOL
    &ff_udp_protocol,
#endif
#if CONFIG_UDPLITE_PROTOCOL
    &ff_udplite_protocol,
#endif
#if CONFIG_UNIX_PROTOCOL
    &ff_unix_protocol,
#endif
    NULL };
