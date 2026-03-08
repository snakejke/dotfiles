#!/bin/sh

#SAVEDIR="/mnt/nas/recordings"
SAVEDIR="$HOME"

getdim() { xrandr | grep -oP '(?<=current ).*(?=,)' | tr -d ' ' ;}

updateicon() {
    if [ "$1" = "" ]; then
        notify-send -u low "🛑 Recording stopped" "File saved to $SAVEDIR"
    else
        notify-send -u low "🔴 Recording started" "$1"
    fi
}

killrecording() {
    recpid="$(cat /tmp/recordingpid 2>/dev/null)"
    if [ -n "$recpid" ]; then
        kill -15 "$recpid" 2>/dev/null
        pkill -P "$recpid" 2>/dev/null
        rm -f /tmp/recordingpid
    fi
    if [ -f /tmp/overlaypid ]; then
        overlaypid="$(cat /tmp/overlaypid)"
        kill "$overlaypid" 2>/dev/null
        rm -f /tmp/overlaypid
    fi
    updateicon ""
    exit 0
}

screencast() {
    ffmpeg -y \
    -f x11grab \
    -framerate 30 \
    -s "$(getdim)" \
    -i "$DISPLAY" \
    -r 24 \
    -use_wallclock_as_timestamps 1 \
    -f alsa -thread_queue_size 1024 -i default \
    -c:v h264 \
    -crf 0 -preset ultrafast -c:a aac \
    "$SAVEDIR/screencast-$(date '+%y%m%d-%H%M-%S').mp4" &
    echo $! > /tmp/recordingpid
    updateicon "Screen+Audio"
}

video() {
    ffmpeg \
    -f x11grab \
    -framerate 30 \
    -s "$(getdim)" \
    -i "$DISPLAY" \
    -c:v libx264 -qp 0 -r 30 \
    "$SAVEDIR/video-$(date '+%y%m%d-%H%M-%S').mkv" &
    echo $! > /tmp/recordingpid
    updateicon "Screen only"
}

webcamhidef() {
    ffmpeg \
    -f v4l2 \
    -i /dev/video0 \
    -video_size 1920x1080 \
    "$SAVEDIR/webcam-$(date '+%y%m%d-%H%M-%S').mkv" &
    echo $! > /tmp/recordingpid
    updateicon "Webcam HD"
}

webcam() {
    ffmpeg \
    -f v4l2 \
    -i /dev/video0 \
    -video_size 640x480 \
    "$SAVEDIR/webcam-$(date '+%y%m%d-%H%M-%S').mkv" &
    echo $! > /tmp/recordingpid
    updateicon "Webcam"
}

audio() {
    ffmpeg \
    -f alsa -i default \
    -c:a flac \
    "$SAVEDIR/audio-$(date '+%y%m%d-%H%M-%S').flac" &
    echo $! > /tmp/recordingpid
    updateicon "Audio"
}

videoselected() {
    slop -f "%x %y %w %h" > /tmp/slop
    read -r X Y W H < /tmp/slop
    rm /tmp/slop
    overlay_frame "$X" "$Y" "$W" "$H" &
    echo $! > /tmp/overlaypid
    ffmpeg \
    -f x11grab \
    -framerate 30 \
    -video_size "${W}x${H}" \
    -i "$DISPLAY+$X,$Y" \
    -c:v libx264 -qp 0 -r 30 \
    "$SAVEDIR/box-$(date '+%y%m%d-%H%M-%S').mkv" &
    echo $! > /tmp/recordingpid
    updateicon "Selected area"
}

askrecording() {
    choice=$(printf "screencast\\nvideo\\nvideo selected\\naudio\\nwebcam\\nwebcam (hi-def)" | rofi -dmenu -i -p "Select recording style:")
    case "$choice" in
        screencast) screencast;;
        audio) audio;;
        video) video;;
        *selected) videoselected;;
        webcam) webcam;;
        "webcam (hi-def)") webcamhidef;;
    esac
}

mkdir -p "$SAVEDIR"

[ -z "$DISPLAY" ] && export DISPLAY=":0"

case "$1" in
    screencast) screencast;;
    audio) audio;;
    video) video;;
    *selected) videoselected;;
    kill) killrecording;;
    *)
        if [ -f /tmp/recordingpid ]; then
            killrecording
        else
            askrecording
        fi
        ;;
esac
