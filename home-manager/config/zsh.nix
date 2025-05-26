{ config, lib, pkgs, ... }:

{
programs.zsh = {
	enable = true;
  dotDir = ".config/zsh";

  history = {
    size = 50000;
    save = 50000;
    path = "${config.xdg.configHome}/zsh/.zhistory";
    extended = true;           # setopt extended_history
    ignoreDups = true;         # setopt hist_ignore_all_dups  
    ignoreSpace = true;        # setopt hist_ignore_space
    share = true;             # setopt share_history
    expireDuplicatesFirst = false;
  };

  autocd = true;              # setopt autocd
  enableCompletion = true;    # включает compinit
  defaultKeymap = "emacs";    # bindkey -e

  
  profileExtra = ''
     if [[ -z ''${DISPLAY} && ''${XDG_VTNR} -eq 1 ]]; then
        exec sx
     fi
     '';
  
  # Используем initContent с правильным порядком
  initContent = lib.mkMerge [
    # Ранняя инициализация (500 - mkBefore)
    (lib.mkOrder 500 ''
      # zmodload zsh/zprof 
      autoload -U colors && colors
      PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "


      # GPG TTY
      if [[ -z "$GPG_TTY" ]]; then
          export GPG_TTY=$(tty)
          gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1
      fi
    '')
    
    # До completion init (550)
    (lib.mkOrder 550 ''
      zstyle :compinstall filename "$ZDOTDIR/.zshrc"
    '')
    
    # Основная конфигурация (1000 - default)
    (lib.mkOrder 1000 ''
      # Дополнительные keybindings
      bindkey '^ ' autosuggest-accept
      
      # Completion настройки
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
      zstyle ':completion:*' menu select
      
      # Дополнительные опции истории (которых нет в встроенных)
      setopt append_history
      setopt inc_append_history
      setopt hist_find_no_dups
      setopt hist_reduce_blanks
      HIST_STAMPS="%F %T"
      
      # Функции
      download() { aria2c -d ~/Downloads "$1"; }
      
      nvm() {
        unset -f nvm  
        source "$$NVM_DIR/nvm.sh"
        nvm "$$@"
      }
      
      # SBT конфигурация  
      sbt_opts_arr=(
        -Dsbt.ivy.home="''${XDG_CACHE_HOME}/ivy/"
        -Dsbt.boot.directory="''${XDG_CACHE_HOME}/sbt/boot"
        -Dsbt.preloaded="''${XDG_CACHE_HOME}/sbt/preloaded"
        -Dsbt.global.base="''${XDG_CACHE_HOME}/sbt"
        -Dsbt.global.zinc="''${XDG_CACHE_HOME}/sbt/zinc"
        -Dsbt.global.staging="''${XDG_CACHE_HOME}/sbt/staging"
        -Dsbt.global.plugins="''${XDG_CONFIG_HOME}/sbt/plugins"
        -Dsbt.global.settings="''${XDG_CONFIG_HOME}/sbt/global"
        -Dsbt.dependency.base="''${XDG_CACHE_HOME}/sbt/dependency"
        -Dsbt.repository.config="''${XDG_CONFIG_HOME}/sbt/repositories"
        -Divy.home="''${XDG_CACHE_HOME}/ivy"
        -Divy.cache.home="''${XDG_CACHE_HOME}/ivy"
        -Divy.settings.dir="''${XDG_CONFIG_HOME}/ivy"
        -DG8_HOME="''${XDG_CACHE_HOME}/g8/"
      )
      export SBT_OPTS="''${sbt_opts_arr}"
    '')
    
    # Поздняя инициализация (1500 - mkAfter)
    (lib.mkOrder 1500 ''
      # SDKMAN в конце, чтобы не мешать другим настройкам
      [[ -s "${config.home.homeDirectory}/.local/devjava/sdkman/bin/sdkman-init.sh" ]] && source "${config.home.homeDirectory}/.local/devjava/sdkman/bin/sdkman-init.sh"
      
      #zprof
    '')
  ];
  
	  shellAliases = {
      sss = "loginctl poweroff";
      rrr = "loginctl reboot";
      "sudo!" = "sudo $(fc -ln -1)";
      history = "fc -t '%Y-%m-%d %T' -il 1";
      calc = "emacsclient -c -n -e \"(full-calc)\"";
      d = "emacsclient -e \"(dired default-directory)\" -c -a \"\"";
      e = "emacsclient -c -n -a \"\"";
      #e = "emacsclient -c -n -a \"\" -F '((width . 100) (height . 26) (top . 50) (left . -50))'";
      du = "gdu";
      pt = "podman-tui";
      pg = "podman exec -it postgres psql -U postgres -d postgres";
      ip = "ip -c=always";
      ls = "ls -A --group-directories-first --color=auto";
      lf = "lfrun";
      sbt = "sbt -ivy \"$XDG_DATA_HOME/ivy2\" -sbt-dir \"$XDG_DATA_HOME/sbt\"";
      lazypodman = "DOCKER_HOST=unix:///run/user/1000/podman/podman.sock lazydocker";
      pn = "pnpm";
      # scala = "scala $SCALA_REPL_OPTS"; # если $SCALA_REPL_OPTS есть в env
    };
  };

home.file."${config.programs.zsh.dotDir}/.zshenv".text = lib.mkForce ''
# Environment variables
. "${config.xdg.stateHome}/nix/profiles/profile/etc/profile.d/hm-session-vars.sh"
#. "/home/snake/.nix-profile/etc/profile.d/hm-session-vars.sh"

# Only source this once
if [[ -z "$__HM_ZSH_SESS_VARS_SOURCED" ]]; then
  export __HM_ZSH_SESS_VARS_SOURCED=1
fi
  # дальше — ваш собственный код
  #export ZDOTDIR=${config.programs.zsh.dotDir}
  #source ${config.programs.zsh.dotDir}/.zshenv
  #typeset -U PATH
  path=(~/.local/bin/**/*(N-/) ~/.local/bin $path)

  export RANGER_LOAD_DEFAULT_RC=FALSE
  export NO_AT_BRIDGE=1
  export XDG_CONFIG_HOME="$HOME/.config"
  export XDG_DATA_HOME="$HOME/.local/share" 
  export XDG_CACHE_HOME="$HOME/.cache"
  export XDG_RUNTIME_DIR=/run/user/$(id -u)
  export XDG_STATE_HOME="$HOME/.local/state"
  export XDG_CONFIG_DIRS=/etc/xdg
  export DEV_HOME="$HOME/.local/devjava"
  export SDKMAN_DIR="$DEV_HOME/sdkman"
  export FVWM_USERDIR="$XDG_CONFIG_HOME/fvwm"
  export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
  export CUDA_CACHE_PATH="$XDG_CACHE_HOME/.nv/ComputeCache"
  path=(/usr/local/cuda/bin $path)
  export MYSQL_HISTFILE="$XDG_DATA_HOME/mysql_history"  
  export PSQL_HISTORY="$XDG_STATE_HOME/psql_history"
  path=(/usr/lib/psql16/bin $path)
  export JAVA_HOME="$SDKMAN_DIR/candidates/java/current"
  path=($SDKMAN_DIR/candidates/java/current/bin $path)
  export ANDROID_USER_HOME="$DEV_HOME/android-sdk"
  export ANDROID_HOME="$DEV_HOME/android-sdk"
  export ANDROID_SDK_ROOT="$DEV_HOME/android-sdk"
  export ANDROID_SDK_HOME="$DEV_HOME/android-sdk"
  path=($ANDROID_HOME/cmdline-tools/latest/bin $path)
  path=($ANDROID_HOME/platform-tools $path)
  path=($ANDROID_HOME/emulator $path)
  path=($DEV_HOME/clojure/bin $path)
  export AWT_TOOLKIT=MToolkit
  export _JAVA_AWT_WM_NONREPARENTING=1  
  path=($SDKMAN_DIR/candidates/maven/current/bin $path)
  export GRADLE_USER_HOME="$DEV_HOME/.gradle"
  export PASSWORD_STORE_ENABLE_EXTENSIONS=true
  export PASSWORD_STORE_EXTENSIONS_DIR="$HOME/.local/lib/python3.10/site-packages/usr/lib/password-store/extensions/"
  export PASSWORD_STORE_DIR="$HOME/.password-store"
  export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/.notmuch-config"
  export W3M_DIR="$XDG_DATA_HOME/w3m"
  export GOPATH="$XDG_DATA_HOME/go"
  path=($XDG_DATA_HOME/go/bin $path)
  export CARGO_HOME="$XDG_DATA_HOME/cargo"
  path=($XDG_DATA_HOME/cargo/bin $path)
  export RUSTUP_HOME="$XDG_DATA_HOME/.rustup"
  export IPYTHONDIR="$XDG_CONFIG_HOME/ipython"
  export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
  export RUFF_CACHE_DIR="$XDG_CACHE_HOME/.ruff_cache"
  path=($XDG_DATA_HOME/coursier/bin $path) 
  path=($XDG_DATA_HOME/mise/shims $path) 
  export MIX_XDG=1
  export GHCUP_USE_XDG_DIRS=1
  export STACK_XDG=1
  export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker/config"
  export MINIKUBE_HOME="$XDG_DATA_HOME/minikube"
  export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
  export PERL_CPANM_HOME="$HOME/.local/etc/cpanm"
  export HISTFILE="$XDG_CONFIG_HOME/zsh/.zhistory"
  export HISTSIZE=50000
  export SAVEHIST=$HISTSIZE
  export NODE_REPL_HISTORY="$XDG_CONFIG_HOME/nodejs/.node_repl_history"
  export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
  export NVM_DIR="$XDG_CONFIG_HOME/nvm"
  export PNPM_HOME="$XDG_DATA_HOME/pnpm"
  path=($PNPM_HOME $path)
  path=($XDG_CONFIG_HOME/npm/node_modules/bin $path)
  export EDITOR="emacsclient -c -n"
  export VISUAL=$EDITOR
  export ANSIBLE_HOME="$XDG_CONFIG_HOME/ansible"
  export ANSIBLE_CONFIG="$XDG_CONFIG_HOME/ansible/ansible.cfg"
  export ANSIBLE_GALAXY_CACHE_DIR="$XDG_CACHE_HOME/ansible/galaxy_cache"
  export RESTIC_PASSWORD_FILE="$PASSWORD_STORE_DIR/.restic"
  export EMACS_SOCKET_NAME="/run/user/1000/emacs/vanilla"
  export LSP_USE_PLISTS=true
  export TMUX_CONF="$HOME/.config/tmux/tmux.conf"
  export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
  export LYNX_CFG="$HOME/.config/lynx/lynx.cfg" 
  export LESSHISTFILE=/tmp/.lesshst
  export LESSKEY=/tmp/.lesskey
  '';
}
