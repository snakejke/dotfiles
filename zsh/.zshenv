typeset -U PATH
path=(~/.local/bin/**/*(N-/) ~/.local/bin $path)
#
export RANGER_LOAD_DEFAULT_RC=FALSE
# fix noise chrome&gtk 
export NO_AT_BRIDGE=1
#
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share" 
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_RUNTIME_DIR=/run/user/$(id -u)
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CONFIG_DIRS=/etc/xdg
export DEV_HOME="$HOME/.local/devjava"
export SDKMAN_DIR="$DEV_HOME/sdkman"
export SDKMAN_CANDIDATES_DIR="$SDKMAN_DIR/candidates"
#
export FVWM_USERDIR="$XDG_CONFIG_HOME/fvwm"
#
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
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
#maven + gradle
export MAVEN_HOME="$SDKMAN_DIR/candidates/maven/current"
export MVND_HOME="$SDKMAN_DIR/candidates/mvnd/current"
path=($MAVEN_HOME/bin $path)
export GRADLE_USER_HOME="$DEV_HOME/.gradle"
export GRADLE_HOME="$SDKMAN_DIR/candidates/gradle/current"
path=($GRADLE_HOME/bin $path)
#
export LIQUIBASE_HOME="$SDKMAN_DIR/candidates/liquibase/current"
export VISUALVM_HOME="$SDKMAN_DIR/candidates/visualvm/current"
#
export ANDROID_USER_HOME="$DEV_HOME/android-sdk"
export ANDROID_HOME="$DEV_HOME/android-sdk"
export ANDROID_SDK_ROOT="$DEV_HOME/android-sdk"
export ANDROID_SDK_HOME="$DEV_HOME/android-sdk"
path=($ANDROID_HOME/cmdline-tools/latest/bin $path)
path=($ANDROID_HOME/platform-tools $path)
path=($ANDROID_HOME/emulator $path)
#Clojure
export LEIN_HOME="$XDG_CACHE_HOME/leiningen"
#
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
#Pass
export PASSWORD_STORE_ENABLE_EXTENSIONS=true
export PASSWORD_STORE_EXTENSIONS_DIR="$XDG_STATE_HOME/nix/profile/lib/password-store/extensions"
export PASSWORD_STORE_DIR="$HOME/.password-store"
#
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/.notmuch-config"
#
export W3M_DIR="$XDG_DATA_HOME/w3m"
export PARALLEL_HOME="$XDG_CONFIG_HOME/parallel"
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
export PYTHON_HISTORY="$XDG_STATE_HOME/.python_history"
export RUFF_CACHE_DIR="$XDG_CACHE_HOME/.ruff_cache"
#Scala
_sbt_opts=(
  "-Dsbt.global.base=$XDG_DATA_HOME/sbt"
  "-Dsbt.ivy.home=$XDG_CACHE_HOME/ivy2"
  "-DG8_HOME=$XDG_DATA_HOME/sbt/.g8"
)
export SBT_OPTS="${_sbt_opts[*]}"
unset _sbt_opts
#mise Erlang/Elixir/Ruby 
path=($XDG_DATA_HOME/mise/shims $path)
export MIX_XDG=1
#Haskell
export GHCUP_USE_XDG_DIRS=1
export STACK_XDG=1
#nimble
path=($XDG_DATA_HOME/nimble/bin $path)
#
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker/config"
export MINIKUBE_HOME="$XDG_DATA_HOME/minikube"
export KUBECONFIG="$XDG_CONFIG_HOME/kube/config" 
export KUBECACHEDIR="$XDG_CACHE_HOME/kube"
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
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export NVM_DIR="$XDG_CONFIG_HOME/nvm"
export PNPM_HOME="$XDG_DATA_HOME/pnpm"
path=($PNPM_HOME $path)
path=($XDG_CONFIG_HOME/npm/node_modules/bin $path)
#
export EDITOR="emacsclient-wrapper"
export VISUAL=$EDITOR
#Ansible
export ANSIBLE_HOME="$XDG_CONFIG_HOME/ansible"
export ANSIBLE_CONFIG="$XDG_CONFIG_HOME/ansible/ansible.cfg"
export ANSIBLE_GALAXY_CACHE_DIR="$XDG_CACHE_HOME/ansible/galaxy_cache"
#
export RESTIC_PASSWORD_FILE="$PASSWORD_STORE_DIR/.restic"
# 
export EMACS_SOCKET_NAME="/run/user/1000/emacs/vanilla"
#emacs-lsp
export LSP_USE_PLISTS=true
#
export ZSHZ_DATA="$ZDOTDIR/.z"
#
export XCURSOR_PATH="$HOME/.local/state/nix/profile/share/icons:/usr/share/icons:$HOME/.local/share/icons"
#
export TEXMFHOME="$XDG_DATA_HOME/texmf"
export TEXMFVAR="$XDG_CACHE_HOME/texlive/texmf-var"
export TEXMFCONFIG="$XDG_CONFIG_HOME/texlive/texmf-config"
#
export TMUX_CONF=~/.config/tmux/tmux.conf
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export LYNX_CFG=~/.config/lynx/lynx.cfg
export LESSHISTFILE=/tmp/.lesshst
export LESSKEY=/tmp/.lesskey
export NLTK_DATA="$XDG_DATA_HOME/nltk_data"
