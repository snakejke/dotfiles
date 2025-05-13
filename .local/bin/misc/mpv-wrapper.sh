#!/bin/sh
export LD_PRELOAD=/usr/lib/libvulkan.so.1
exec /usr/bin/mpv "$@"
