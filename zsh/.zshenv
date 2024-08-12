typeset -U PATH
export PATH="$PATH:${$(find ~/.local/bin -type d -printf %p:)%%:}"
#
#export FONTCONFIG_FILE=/etc/fonts/fonts.conf
#export FONTCONFIG_PATH=/etc/fonts/
#
export RANGER_LOAD_DEFAULT_RC=FALSE
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share" 
export XDG_CACHE_HOME="$HOME/.cache"

export XDG_RUNTIME_DIR=/run/user/$(id -u)
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_DATA_DIRS="/usr/share:/usr/local/share:/var/lib/flatpak/exports/share:/home/snake/.local/share/flatpak/exports/share"
export FVWM_USERDIR="$XDG_CONFIG_HOME/fvwm"
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
#c vpdau были проблемы. пробуем nvidia
#export LIBVA_DRIVER_NAME=nvidia
#export MOZ_DISABLE_RDD_SANDBOX=1
#export NVD_BACKEND=direct
#
#export ICEAUTHORITY="$XDG_CACHE_HOME"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

#fix  ~/.nv/ComputeCache
export CUDA_CACHE_PATH="$XDG_CACHE_HOME/.nv/ComputeCache"

export MYSQL_HISTFILE="$XDG_DATA_HOME/mysql_history"
export PSQL_HISTORY="$XDG_STATE_HOME/psql_history"

export PATH="${PATH}:/usr/lib/psql16/bin"
#SDKMAN
#export JAVA_HOME=/opt/jdk19
export PATH="${PATH}:$JAVA_HOME/bin"
#SDKMAN
export PATH="${PATH}:$HOME/.local/devjava/sdkman/candidates/maven/current/bin"
#if need Doom
#export PATH="$HOME/.config/emacs/bin:$PATH"
#Pass
export PASSWORD_STORE_ENABLE_EXTENSIONS=true
export PASSWORD_STORE_EXTENSIONS_DIR="$HOME/.local/lib/python3.10/site-packages/usr/lib/password-store/extensions/"
export PASSWORD_STORE_DIR="$HOME/.password-store"
#
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/.notmuch-config"
#
export W3M_DIR="$XDG_DATA_HOME/w3m"
#
export ANDROID_USER_HOME="$HOME/.local/devjava/.android"
#
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
#
export PATH="$PATH:/home/snake/.local/share/coursier/bin"
#Go
export GOPATH="$XDG_DATA_HOME/go"
export PATH="${PATH}:$XDG_DATA_HOME/go/bin"
#Rust
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export PATH="${PATH}:$HOME/.local/share/cargo/bin"
#
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/docker
#
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
##
export PERL_CPANM_HOME="$HOME"/.local/etc/cpanm
#broken
#export GTK2_RC_FILES="/home/snake/.config/gtk-2.0/.gtkrc-2.0"
#---
#export CHROMIUM_FLAGS="$XDG_CONFIG_HOME/google-chrome/google-chrome-flags.conf"
export HISTFILE="$XDG_CONFIG_HOME/zsh/.zhistory"
export HISTSIZE=50000
export SAVEHIST=$HISTSIZE

export NODE_REPL_HISTORY="$XDG_CONFIG_HOME/nodejs/.node_repl_history"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export NVM_DIR="$XDG_CONFIG_HOME/nvm"
#Chrome ignoring that 
export NSS_DEFAULT_DB_DIR="$HOME/.local/.pki/nssdb"
export EDITOR="emacsclient -c -n"
export VISUAL=$EDITOR
#Ansible
export ANSIBLE_HOME="$XDG_CONFIG_HOME/ansible"
export ANSIBLE_CONFIG="$XDG_CONFIG_HOME/ansible/ansible.cfg"
export ANSIBLE_GALAXY_CACHE_DIR="$XDG_CACHE_HOME/ansible/galaxy_cache"

export EMACS_SOCKET_NAME="/run/user/1000/emacs/snake-emacsd-vanilla"
export TMUX_CONF=~/.config/tmux/tmux.conf
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export LYNX_CFG=~/.config/lynx/lynx.cfg
export LESSHISTFILE=/tmp/.lesshst
export LESSKEY=/tmp/.lesskey
