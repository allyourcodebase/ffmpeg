#include "config_components.h"
static const URLProtocol * const url_protocols[] = {
#if CONFIG_ANDROID_CONTENT_PROTOCOL
    &ff_android_content_protocol,
#endif
#if CONFIG_ASYNC_PROTOCOL
    &ff_async_protocol,
#endif
#if CONFIG_BLURAY_PROTOCOL
    &ff_bluray_protocol,
#endif
#if CONFIG_CACHE_PROTOCOL
    &ff_cache_protocol,
#endif
#if CONFIG_CONCAT_PROTOCOL
    &ff_concat_protocol,
#endif
#if CONFIG_CONCATF_PROTOCOL
    &ff_concatf_protocol,
#endif
#if CONFIG_CRYPTO_PROTOCOL
    &ff_crypto_protocol,
#endif
#if CONFIG_DATA_PROTOCOL
    &ff_data_protocol,
#endif
#if CONFIG_FD_PROTOCOL
    &ff_fd_protocol,
#endif
#if CONFIG_FFRTMPCRYPT_PROTOCOL
    &ff_ffrtmpcrypt_protocol,
#endif
#if CONFIG_FFRTMPHTTP_PROTOCOL
    &ff_ffrtmphttp_protocol,
#endif
#if CONFIG_FILE_PROTOCOL
    &ff_file_protocol,
#endif
#if CONFIG_FTP_PROTOCOL
    &ff_ftp_protocol,
#endif
#if CONFIG_GOPHER_PROTOCOL
    &ff_gopher_protocol,
#endif
#if CONFIG_GOPHERS_PROTOCOL
    &ff_gophers_protocol,
#endif
#if CONFIG_HLS_PROTOCOL
    &ff_hls_protocol,
#endif
#if CONFIG_HTTP_PROTOCOL
    &ff_http_protocol,
#endif
#if CONFIG_HTTPPROXY_PROTOCOL
    &ff_httpproxy_protocol,
#endif
#if CONFIG_HTTPS_PROTOCOL
    &ff_https_protocol,
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
#if CONFIG_MD5_PROTOCOL
    &ff_md5_protocol,
#endif
#if CONFIG_PIPE_PROTOCOL
    &ff_pipe_protocol,
#endif
#if CONFIG_PROMPEG_PROTOCOL
    &ff_prompeg_protocol,
#endif
#if CONFIG_RTMP_PROTOCOL
    &ff_rtmp_protocol,
#endif
#if CONFIG_RTMPE_PROTOCOL
    &ff_rtmpe_protocol,
#endif
#if CONFIG_RTMPS_PROTOCOL
    &ff_rtmps_protocol,
#endif
#if CONFIG_RTMPT_PROTOCOL
    &ff_rtmpt_protocol,
#endif
#if CONFIG_RTMPTE_PROTOCOL
    &ff_rtmpte_protocol,
#endif
#if CONFIG_RTMPTS_PROTOCOL
    &ff_rtmpts_protocol,
#endif
#if CONFIG_RTP_PROTOCOL
    &ff_rtp_protocol,
#endif
#if CONFIG_SCTP_PROTOCOL
    &ff_sctp_protocol,
#endif
#if CONFIG_SRTP_PROTOCOL
    &ff_srtp_protocol,
#endif
#if CONFIG_SUBFILE_PROTOCOL
    &ff_subfile_protocol,
#endif
#if CONFIG_TEE_PROTOCOL
    &ff_tee_protocol,
#endif
#if CONFIG_TCP_PROTOCOL
    &ff_tcp_protocol,
#endif
#if CONFIG_TLS_PROTOCOL
    &ff_tls_protocol,
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
#if CONFIG_LIBAMQP_PROTOCOL
    &ff_libamqp_protocol,
#endif
#if CONFIG_LIBRIST_PROTOCOL
    &ff_librist_protocol,
#endif
#if CONFIG_LIBRTMP_PROTOCOL
    &ff_librtmp_protocol,
#endif
#if CONFIG_LIBRTMPE_PROTOCOL
    &ff_librtmpe_protocol,
#endif
#if CONFIG_LIBRTMPS_PROTOCOL
    &ff_librtmps_protocol,
#endif
#if CONFIG_LIBRTMPT_PROTOCOL
    &ff_librtmpt_protocol,
#endif
#if CONFIG_LIBRTMPTE_PROTOCOL
    &ff_librtmpte_protocol,
#endif
#if CONFIG_LIBSRT_PROTOCOL
    &ff_libsrt_protocol,
#endif
#if CONFIG_LIBSSH_PROTOCOL
    &ff_libssh_protocol,
#endif
#if CONFIG_LIBSMBCLIENT_PROTOCOL
    &ff_libsmbclient_protocol,
#endif
#if CONFIG_LIBZMQ_PROTOCOL
    &ff_libzmq_protocol,
#endif
#if CONFIG_IPFS_GATEWAY_PROTOCOL
    &ff_ipfs_gateway_protocol,
#endif
#if CONFIG_IPNS_GATEWAY_PROTOCOL
    &ff_ipns_gateway_protocol,
#endif
    NULL };
