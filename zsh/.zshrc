autoload -U colors && colors
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "


export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1
bindkey -e

#if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
#    tmux attach -t default || tmux new -s default
#fi

zstyle :compinstall filename "$ZDOTDIR/.zshrc"
export MANPAGER="BATPAGER"
#export $(dbus-launch)

autoload -Uz compinit
compinit

. "$ZDOTDIR/aliases"
. "$ZDOTDIR/.functions"
. "$ZDOTDIR/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh"

setopt append_history     
setopt inc_append_history
setopt hist_ignore_all_dups
setopt share_history


mkfile() {
    mkdir -p "$(dirname "$1")" && touch "$1"
    ${EDITOR:-nvim} "$1"
}

download() { aria2c -d ~/Downloads "$1"; }


nvm() {
  unset -f nvm  
  source "$NVM_DIR/nvm.sh"
  nvm "$@"
}

export SDKMAN_DIR="/home/snake/.local/devjava/sdkman"
# sdk() {
#   unset -f sdk
#   source "$SDKMAN_DIR/bin/sdkman-init.sh"
#   sdk "$@"
# }
[[ -s "/home/snake/.local/devjava/sdkman/bin/sdkman-init.sh" ]] && source "/home/snake/.local/devjava/sdkman/bin/sdkman-init.sh"
