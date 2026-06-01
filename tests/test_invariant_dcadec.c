#include <check.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>

START_TEST(test_dca_decoder_buffer_safety)
{
    /* Invariant: DCA decoder must not crash or overflow when given
       adversarial input_size values including near-INT_MAX boundaries */

    /* Payloads: crafted raw DCA sync-word headers with adversarial sizes */
    static const uint8_t exploit_payload[16] = {
        0x7F, 0xFE, 0x80, 0x01,  /* DCA sync word */
        0xFF, 0xFF, 0xFF, 0xFF,  /* near-max size fields */
        0xFF, 0xFF, 0xFF, 0xFF,
        0x00, 0x00, 0x00, 0x00
    };
    static const uint8_t boundary_payload[16] = {
        0x7F, 0xFE, 0x80, 0x01,  /* DCA sync word */
        0x7F, 0xFF, 0xFF, 0xFF,  /* INT_MAX/2 boundary */
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00
    };
    static const uint8_t valid_payload[16] = {
        0x7F, 0xFE, 0x80, 0x01,  /* DCA sync word */
        0x00, 0x00, 0x00, 0x10,  /* small valid size */
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00
    };

    const uint8_t *payloads[] = { exploit_payload, boundary_payload, valid_payload };
    const int payload_sizes[] = { 16, 16, 16 };
    int num_payloads = 3;

    const AVCodec *codec = avcodec_find_decoder(AV_CODEC_ID_DTS);
    ck_assert_msg(codec != NULL, "DTS/DCA codec not found");

    for (int i = 0; i < num_payloads; i++) {
        AVCodecContext *ctx = avcodec_alloc_context3(codec);
        ck_assert_msg(ctx != NULL, "Failed to allocate codec context");

        int ret = avcodec_open2(ctx, codec, NULL);
        ck_assert_msg(ret >= 0, "Failed to open codec");

        AVPacket *pkt = av_packet_alloc();
        ck_assert_msg(pkt != NULL, "Failed to allocate packet");

        ret = av_new_packet(pkt, payload_sizes[i]);
        ck_assert_msg(ret == 0, "Failed to create packet");
        memcpy(pkt->data, payloads[i], payload_sizes[i]);

        AVFrame *frame = av_frame_alloc();
        ck_assert_msg(frame != NULL, "Failed to allocate frame");

        /* Must not crash or cause memory corruption */
        avcodec_send_packet(ctx, pkt);
        avcodec_receive_frame(ctx, frame);

        av_frame_free(&frame);
        av_packet_free(&pkt);
        avcodec_free_context(&ctx);
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s = suite_create("Security");
    TCase *tc_core = tcase_create("Core");
    tcase_set_timeout(tc_core, 10);
    tcase_add_test(tc_core, test_dca_decoder_buffer_safety);
    suite_add_tcase(s, tc_core);
    return s;
}

int main(void)
{
    int number_failed;
    Suite *s = security_suite();
    SRunner *sr = srunner_create(s);
    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}