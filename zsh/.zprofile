
#Ctrl+C 
trap 'exit' SIGINT SIGTERM

if [[ -z ${DISPLAY} && ${XDG_VTNR} -eq 1 ]]; then
   exec sx
fi
