# FFmpeg Packaged for Zig

This is a fork of [ffmpeg](https://ffmpeg.org/), packaged for Zig. Unnecessary
files have been deleted, and the build system has been replaced with
`build.zig`. The `upstream` branch of this repository contains a mirror of the
official ffmpeg source tree. When an upstream release is tagged, the changes are
merged from `upstream` into this `main` branch.

## Update Process

1. Merge the new tag into main and resolve all conflicts by replacing the
   conflicting files with the files from upstream.
2. `find libavcodec/ libavdevice/ libavfilter/ libavformat libavutil/ libswscale/ libswresample/ -type f -name "*.asm" -o -name "*.c" -o -name "*.S"`
   * Edit to omit files ending in `_template.c`
   * Sort the list
   * Update the `all_sources` list in `build.zig`.
3. Inspect the git diff to keep some of the source files commented out like
   they were before. Some handy filtering rules apply:
   * `/L` prefix means Linux-only
   * `/W` prefix means Windows-only
