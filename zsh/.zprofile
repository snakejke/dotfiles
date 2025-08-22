 if [[ -z ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
     exec sx 2>&1 | logger -p local1.info -t "sx-session"
 fi

#if [[ -z ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
#        exec sx > ~/.local/var/log/Xorg.log 2>&1
#fi
