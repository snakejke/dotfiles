#!/bin/sh
# Usage:
# $0: Ask for recording type via rofi
# $0 screencast: Record both audio and screen
# $0 video: Record only screen
# $0 audio: Record only audio
# $0 kill: Kill existing recording
#
# If there is already a running instance, user will be prompted to end it.
getdim() { xrandr | grep -oP '(?<=current ).*(?=,)' | tr -d ' ' ;}

updateicon() { \
    if [ "$1" = "" ]; then
        notify-send -u low "Recording stopped"
    else
        notify-send -u low "Recording" "Recording started $1"
    fi
}

killrecording() {
    recpid="$(cat /tmp/recordingpid)"
    # Убиваем сам процесс ffmpeg
    kill -15 "$recpid"
    # Убиваем дочерние процессы
    pkill -P "$recpid"
    rm -f /tmp/recordingpid
    updateicon ""
    exit 0
}

screencast() { \
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
    "$HOME/screencast-$(date '+%y%m%d-%H%M-%S').mp4" &
    echo $! > /tmp/recordingpid
    updateicon "Screen+Audio"
}

video() { ffmpeg \
    -f x11grab \
    -framerate 30 \
    -s "$(getdim)" \
    -i "$DISPLAY" \
    -c:v libx264 -qp 0 -r 30 \
    "$HOME/video-$(date '+%y%m%d-%H%M-%S').mkv" &
    echo $! > /tmp/recordingpid
    updateicon "Screen only"
}

webcamhidef() { ffmpeg \
    -f v4l2 \
    -i /dev/video0 \
    -video_size 1920x1080 \
    "$HOME/webcam-$(date '+%y%m%d-%H%M-%S').mkv" &
    echo $! > /tmp/recordingpid
    updateicon "Webcam HD"
}

webcam() { ffmpeg \
    -f v4l2 \
    -i /dev/video0 \
    -video_size 640x480 \
    "$HOME/webcam-$(date '+%y%m%d-%H%M-%S').mkv" &
    echo $! > /tmp/recordingpid
    updateicon "Webcam"
}

audio() { \
    ffmpeg \
    -f alsa -i default \
    -c:a flac \
    "$HOME/audio-$(date '+%y%m%d-%H%M-%S').flac" &
    echo $! > /tmp/recordingpid
    updateicon "Audio"
}

askrecording() { \
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

asktoend() { \
    response=$(printf "No\\nYes" | rofi -dmenu -i -p "Recording still active. End recording?") &&
    [ "$response" = "Yes" ] &&  killrecording
}

videoselected()
{
    slop -f "%x %y %w %h" > /tmp/slop
    read -r X Y W H < /tmp/slop
    rm /tmp/slop
    ffmpeg \
    -f x11grab \
    -framerate 30 \
    -video_size "$W"x"$H" \
    -i "$DISPLAY+$X,$Y" \
    -c:v libx264 -qp 0 -r 30 \
    "$HOME/box-$(date '+%y%m%d-%H%M-%S').mkv" &
    echo $! > /tmp/recordingpid
    updateicon "Selected area"
}

# Убедимся, что DISPLAY установлен
[ -z "$DISPLAY" ] && export DISPLAY=":0"

case "$1" in
    screencast) screencast;;
    audio) audio;;
    video) video;;
    *selected) videoselected;;
    kill) killrecording;;
    *) ([ -f /tmp/recordingpid ] && asktoend && exit) || askrecording;;
esac
