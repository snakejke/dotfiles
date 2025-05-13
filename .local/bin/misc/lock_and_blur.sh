#!/usr/bin/env bash

# Kill the GPG agent and start a new one
gpg-connect-agent killagent /bye
gpg-connect-agent /bye

# Configuration
icon="$HOME/Documents/Pictures/lock_icons/lock.png"
tmpbg="/tmp/screen_$(date +%s).png"  # Unique temporary file
lockargs=()                          # Array for i3lock arguments

cleanup() {
    rm -f "$tmpbg"  # Remove temporary file on script exit
}

trap cleanup EXIT  # Ensure cleanup happens even if script fails

# Take a fresh screenshot
if ! scrot "$tmpbg"; then
    echo "Failed to take screenshot"
    exit 1
fi

# Apply blur and overlay icon
if magick "$tmpbg" -filter Gaussian -thumbnail 20% -sample 500% "$tmpbg" && \
   magick "$tmpbg" "$icon" -gravity center -composite "$tmpbg"; then
    lockargs+=(-i "$tmpbg")  # Add image argument if processing succeeded
else
    echo "Image processing failed"
    lockargs+=(-c 000000)    # Fallback to solid color if image processing fails
fi

# Lock screen
i3lock "${lockargs[@]}"
