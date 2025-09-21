#!/bin/bash

/usr/libexec/xfce-polkit &
picom --config ~/.config/picom/picom.conf &
wmname LG3D &
#feh --no-fehbg --bg-scale ~/Documents/Pictures/wallpapers/wall.jpg &
pipewire &
exec fvwm3
