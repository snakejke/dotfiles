#!/bin/bash

# Get current display
DISPLAY_NUM=$(echo $DISPLAY | cut -d ":" -f 2 | cut -d "." -f 1)

# Get screen resolution
read SCREEN_WIDTH SCREEN_HEIGHT << EOF
$(xdpyinfo | awk '/dimensions:/ { print $2 }' | tr 'x' ' ')
EOF

# Get list of windows
WINDOWS=$(wmctrl -l -p -x | while read -r wid pid workspace class host title; do
    # Skip windows without titles
    [ -z "$title" ] && continue
    
    # Get window type and state
    window_type=$(xprop -id "${wid}" _NET_WM_WINDOW_TYPE 2>/dev/null)
    window_state=$(xprop -id "${wid}" _NET_WM_STATE 2>/dev/null)
    
    # Skip certain window types
    [[ "$window_type" == *"_NET_WM_WINDOW_TYPE_DOCK"* ]] && continue
    [[ "$window_type" == *"_NET_WM_WINDOW_TYPE_DESKTOP"* ]] && continue
    
    # Get WM_CLASS for better application name
    wm_class=$(xprop -id "${wid}" WM_CLASS 2>/dev/null | cut -d '"' -f 2)
    
    # Get window name
    wm_name=$(xprop -id "${wid}" _NET_WM_NAME WM_NAME 2>/dev/null | tail -n 1 | cut -d '=' -f 2- | tr -d '"' | sed 's/^ //')
    
    # Output if we have both class and name
    if [ -n "$wm_class" ] && [ -n "$wm_name" ]; then
        echo "${wid}    ${wm_class}: ${wm_name}"
    fi
done)

# Debug output - let's see what windows we found
echo "Available windows:" >&2
echo "$WINDOWS" >&2
echo "---" >&2

# Select window through rofi
CHOSEN=$(echo "$WINDOWS" | rofi -dmenu -i -p "Select Window" | awk '{print $1}')

# Check selection
if [ -z "$CHOSEN" ]; then
    echo "No window selected."
    exit 1
fi

# Get window dimensions and position
eval $(xwininfo -id "$CHOSEN" | awk '
    /Absolute upper-left X/ {printf "X=%d\n", $4}
    /Absolute upper-left Y/ {printf "Y=%d\n", $4}
    /Width/ {printf "WIDTH=%d\n", $2}
    /Height/ {printf "HEIGHT=%d\n", $2}
')

# Ensure coordinates and dimensions are within screen bounds
if [ "$X" -lt 0 ]; then 
    WIDTH=$((WIDTH + X))
    X=0
fi
if [ "$Y" -lt 0 ]; then
    HEIGHT=$((HEIGHT + Y))
    Y=0
fi

# Ensure width and height don't exceed screen boundaries
if [ "$((X + WIDTH))" -gt "$SCREEN_WIDTH" ]; then
    WIDTH=$((SCREEN_WIDTH - X))
fi
if [ "$((Y + HEIGHT))" -gt "$SCREEN_HEIGHT" ]; then
    HEIGHT=$((SCREEN_HEIGHT - Y))
fi

# Adjust dimensions to be even numbers (required for h264)
WIDTH=$((WIDTH - WIDTH % 2))
HEIGHT=$((HEIGHT - HEIGHT % 2))

# Create timestamp-based filename
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
OUTPUT_FILE="screen_recording_${TIMESTAMP}.mp4"

# Debug output
echo "Screen size: ${SCREEN_WIDTH}x${SCREEN_HEIGHT}"
echo "Recording area: ${WIDTH}x${HEIGHT} at position +${X},${Y}"

# Record video using GStreamer
gst-launch-1.0 -e ximagesrc xid=$CHOSEN use-damage=false ! \
    videoconvert ! \
    queue ! \
    x264enc bitrate=3000 speed-preset=veryfast ! \
    mp4mux ! \
    filesink location="$OUTPUT_FILE"
