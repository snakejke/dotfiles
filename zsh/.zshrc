# zmodload zsh/zprof # for benchmakrs 
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
#export $(dbus-launch)
# TODO: fpath ?
. "$ZDOTDIR/aliases"
. "$ZDOTDIR/.functions"
. "$ZDOTDIR/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh"
. "/usr/share/fzf/key-bindings.zsh"
. "$ZDOTDIR/zsh-z/zsh-z.plugin.zsh"

# TODO:
# autoload -Uz compinit
# compinit
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

export SDKMAN_DIR="/home/snake/.local/devjava/sdkman"
# :TODO 
# sdk() {
#   unset -f sdk
#   source "$SDKMAN_DIR/bin/sdkman-init.sh"
#   sdk "$@"
# }
[[ -s "/home/snake/.local/devjava/sdkman/bin/sdkman-init.sh" ]] && source "/home/snake/.local/devjava/sdkman/bin/sdkman-init.sh"
#zprof
