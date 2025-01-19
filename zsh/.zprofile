# if [[ -z ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
#   # Запускаем sx с таймаутом в 10 секунд
#   timeout 10 sx -- -keeptty > /home/snake/.local/var/log/xorg.log 2>&1
#
#   if [[ $? -eq 124 ]]; then
#     echo "Превышено время ожидания запуска X сервера"
#     exit 1
#   fi
# fi
# if [[ -z ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
#    exec sx
# fi
if [[ -z ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
        exec sx > ~/.local/var/log/Xorg.log 2>&1
fi
