# zmodload zsh/zprof 
autoload -U colors && colors
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "

if [[ -z "$GPG_TTY" ]]; then
    export GPG_TTY=$(tty)
    gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1
fi

bindkey '^ ' autosuggest-accept
bindkey -e

zstyle :compinstall filename "$ZDOTDIR/.zshrc"
# TODO: less 
export MANPAGER="BATPAGER"
# TODO: 
. "$ZDOTDIR/aliases"
# . "$ZDOTDIR/.functions"
. "$ZDOTDIR/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh"
. "/usr/share/fzf/key-bindings.zsh"
. "$ZDOTDIR/zsh-z/zsh-z.plugin.zsh"

# TODO:
autoload -Uz +X compinit && compinit

## case insensitive path-completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select

# history
setopt append_history     
setopt extended_history
setopt inc_append_history
setopt hist_ignore_all_dups
setopt hist_find_no_dups
setopt hist_reduce_blanks
setopt hist_ignore_space # 
setopt share_history
HIST_STAMPS="%F %T"
#
setopt autocd

download() { aria2c -d ~/Downloads "$1"; }

nvm() {
  unset -f nvm  
  source "$NVM_DIR/nvm.sh"
  nvm "$@"
}

sbt_opts_arr=(
  -Dsbt.ivy.home="${XDG_CACHE_HOME}/ivy"
  -Dsbt.boot.directory="${XDG_CACHE_HOME}/sbt/boot"
  -Dsbt.preloaded="${XDG_CACHE_HOME}/sbt/preloaded"
  -Dsbt.global.base="${XDG_CACHE_HOME}/sbt"
  -Dsbt.global.zinc="${XDG_CACHE_HOME}/sbt/zinc"
  -Dsbt.global.staging="${XDG_CACHE_HOME}/sbt/staging"
  -Dsbt.global.plugins="${XDG_CONFIG_HOME}/sbt/plugins"
  -Dsbt.global.settings="${XDG_CONFIG_HOME}/sbt/global"
  -Dsbt.dependency.base="${XDG_CACHE_HOME}/sbt/dependency"
  -Dsbt.repository.config="${XDG_CONFIG_HOME}/sbt/repositories"
  -Divy.home="${XDG_CACHE_HOME}/ivy"
  -Divy.cache.home="${XDG_CACHE_HOME}/ivy"
  -Divy.settings.dir="${XDG_CONFIG_HOME}/ivy"
  -DG8_HOME="${XDG_CACHE_HOME}/g8"
)
export SBT_OPTS="${sbt_opts_arr}"

# :TODO 
# sdk() {
#   unset -f sdk
#   source "$SDKMAN_DIR/bin/sdkman-init.sh"
#   sdk "$@"
# }
[[ -s "/home/snake/.local/devjava/sdkman/bin/sdkman-init.sh" ]] && source "/home/snake/.local/devjava/sdkman/bin/sdkman-init.sh"
#zprof
