if [[ -z ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
    exec sx
fi

#exec sx > ~/.local/var/log/xorgstderr.log 2>&1
# if [[ -z ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
#   if [ -x ~/de_wm_chooser.sh ]; then
#     sh ~/de_wm_chooser.sh
#   else
#     exec sx > ~/.local/var/log/xorgstderr.log 2>&1
#   fi
# fi
