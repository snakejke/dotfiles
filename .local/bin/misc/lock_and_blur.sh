#!/usr/bin/env bash

gpg-connect-agent killagent /bye
gpg-connect-agent /bye

icon="$HOME/Documents/Pictures/lock_icons/lock.png"
tmpbg="/tmp/screen_$(date +%s).png"
tmpblur="/tmp/screen_blur_$(date +%s).png"
lockargs=()

cleanup() {
    rm -f "$tmpbg" "$tmpblur"
}
trap cleanup EXIT

rm -f "$tmpbg"

if ! scrot "$tmpbg"; then
    echo "Failed to take screenshot"
    exit 1
fi

if magick "$tmpbg" -filter Gaussian -thumbnail 20% -sample 500% "$tmpblur" && \
   magick "$tmpblur" "$icon" -gravity center -composite "$tmpblur"; then
    lockargs+=(-i "$tmpblur")
else
    echo "Image processing failed"
    lockargs+=(-c 000000)
fi

i3lock "${lockargs[@]}"
