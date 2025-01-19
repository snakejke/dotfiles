#!/bin/bash 
secret-tool lookup name keepass | keepassxc --pw-stdin --keyfile ~/.local/keepassxs/passwords.keyx ~/.local/keepassxs/passwords.kdbx
#sleep 0.3; xdotool key 'Return' | keepassxc --pw-stdin --keyfile ~/.local/keepassxs/passwords.keyx ~/.local/keepassxs/passwords.kdbx
#echo ""
