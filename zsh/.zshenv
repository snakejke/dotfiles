typeset -U PATH
path=(~/.local/bin/**/*(N-/) ~/.local/bin $path)
#
export RANGER_LOAD_DEFAULT_RC=FALSE
# ?
export NO_AT_BRIDGE=1
#
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share" 
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_RUNTIME_DIR=/run/user/$(id -u)
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_DATA_DIRS="/usr/share:/usr/local/share:/var/lib/flatpak/exports/share:/home/snake/.local/share/flatpak/exports/share"
export DEV_HOME="$HOME/.local/devjava"
export SDKMAN_DIR="$DEV_HOME/sdkman"
#
export FVWM_USERDIR="$XDG_CONFIG_HOME/fvwm"
#
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
#c vpdau были проблемы. пробуем nvidia
#export LIBVA_DRIVER_NAME=nvidia
#export MOZ_DISABLE_RDD_SANDBOX=1
#export NVD_BACKEND=direct
#
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
#CUDA
export CUDA_CACHE_PATH="$XDG_CACHE_HOME/.nv/ComputeCache"
path=(/usr/local/cuda/bin $path)
# 
export MYSQL_HISTFILE="$XDG_DATA_HOME/mysql_history"
export PSQL_HISTORY="$XDG_STATE_HOME/psql_history"
path=(/usr/lib/psql16/bin $path)
#java 
export JAVA_HOME="$SDKMAN_DIR/candidates/java/current"
path=($SDKMAN_DIR/candidates/java/current/bin $path)
# /etc/man.conf 
# manpath+=($SDKMAN_DIR/candidates/java/21.0.5.tem/man/man1)
export ANDROID_USER_HOME="$DEV_HOME/android-sdk"
export ANDROID_HOME="$DEV_HOME/android-sdk"
export ANDROID_SDK_ROOT="$DEV_HOME/android-sdk"
export ANDROID_SDK_HOME="$DEV_HOME/android-sdk"
path=($ANDROID_HOME/cmdline-tools/latest/bin $path)
path=($ANDROID_HOME/platform-tools $path)
path=($ANDROID_HOME/emulator $path)
#Clojure
path=($DEV_HOME/clojure/bin $path)
#
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
#maven
path=($SDKMAN_DIR/candidates/maven/current/bin $path)
#GRADLE
export GRADLE_USER_HOME="$DEV_HOME/.gradle"
#Pass
export PASSWORD_STORE_ENABLE_EXTENSIONS=true
export PASSWORD_STORE_EXTENSIONS_DIR="$HOME/.local/lib/python3.10/site-packages/usr/lib/password-store/extensions/"
export PASSWORD_STORE_DIR="$HOME/.password-store"
#
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/.notmuch-config"
#
export W3M_DIR="$XDG_DATA_HOME/w3m"
#Go
export GOPATH="$XDG_DATA_HOME/go"
path=($XDG_DATA_HOME/go/bin $path)
#Rust
export CARGO_HOME="$XDG_DATA_HOME/cargo"
path=($XDG_DATA_HOME/cargo/bin $path)
export RUSTUP_HOME="$XDG_DATA_HOME/.rustup"
#Python
export IPYTHONDIR="$XDG_CONFIG_HOME/ipython"
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
export RUFF_CACHE_DIR="$XDG_CACHE_HOME/.ruff_cache"
#Scala.coursier
#export G8_HOME="$XDG_DATA_HOME/g8"
path=($XDG_DATA_HOME/coursier/bin $path)
#export SCALA_REPL_HISTORY="$HOME/.local/share/scala/.dotty_history"
#export SCALA_REPL_OPTS="-Dscala.color -Dscala.shell.histfile=${SCALA_REPL_HISTORY}"
#mise Erlang/Elixir/Ruby 
path=($XDG_DATA_HOME/mise/shims $path)
export MIX_XDG=1
#Haskell
export GHCUP_USE_XDG_DIRS=1
export STACK_XDG=1
#export CABAL_CONFIG="$XDG_CONFIG_HOME/cabal/config"
#export CABAL_DIR="$XDG_DATA_HOME/cabal"
#
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker/config"
export MINIKUBE_HOME="$XDG_DATA_HOME/minikube"
#
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
#
export PERL_CPANM_HOME="$HOME/.local/etc/cpanm"
#
export HISTFILE="$XDG_CONFIG_HOME/zsh/.zhistory"
export HISTSIZE=50000
export SAVEHIST=$HISTSIZE
# JS 
export NODE_REPL_HISTORY="$XDG_CONFIG_HOME/nodejs/.node_repl_history"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export NVM_DIR="$XDG_CONFIG_HOME/nvm"
export PNPM_HOME="$XDG_DATA_HOME/pnpm"
path=($PNPM_HOME $path)
path=($XDG_CONFIG_HOME/npm/node_modules/bin $path)
#
export EDITOR="emacsclient -c -n"
export VISUAL=$EDITOR
#Ansible
export ANSIBLE_HOME="$XDG_CONFIG_HOME/ansible"
export ANSIBLE_CONFIG="$XDG_CONFIG_HOME/ansible/ansible.cfg"
export ANSIBLE_GALAXY_CACHE_DIR="$XDG_CACHE_HOME/ansible/galaxy_cache"
#
export RESTIC_PASSWORD_FILE="$PASSWORD_STORE_DIR/.restic"
# 
export EMACS_SOCKET_NAME="/run/user/1000/emacs/snake-emacsd-vanilla"
#emacs-lsp
export LSP_USE_PLISTS=true
#
export TMUX_CONF=~/.config/tmux/tmux.conf
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export LYNX_CFG=~/.config/lynx/lynx.cfg
export LESSHISTFILE=/tmp/.lesshst
export LESSKEY=/tmp/.lesskey
