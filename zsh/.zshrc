bindkey -e
bindkey '^ ' autosuggest-accept
bindkey '^Y' copy-line-to-clipboard

setopt PROMPT_SUBST
setopt extendedglob
setopt autocd
setopt extended_history
setopt inc_append_history
setopt hist_ignore_all_dups

if [[ -z "$GPG_TTY" ]]; then
    export GPG_TTY=$(tty)
    gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1
fi

. "$ZDOTDIR/aliases"
. "$ZDOTDIR/fun.zsh"

autoload -U colors && colors
venv_prompt() { [[ $VIRTUAL_ENV ]] && echo "🐍 " }
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
PS1='$(venv_prompt)'"$PS1"

## ⚠️ ## 
export ZSH_DISABLE_COMPFIX=true
export MANPAGER="BATPAGER"
export VIRTUAL_ENV_DISABLE_PROMPT=1
export PROMPT_EOL_MARK='' # hide %

nix_source "zsh-autosuggestions/zsh-autosuggestions.zsh"
nix_source "zsh-z/zsh-z.plugin.zsh"
nix_source "fzf/key-bindings.zsh"

autoload -Uz compinit

local zdump=${ZDOTDIR}/.zcompdump
if [[ ! -f $zdump || $zdump(#qN.mh+24) ]]; then
    compinit -d $zdump
else
    compinit -C -d $zdump
fi

if [[ -f $zdump && ! -f ${zdump}.zwc || $zdump -nt ${zdump}.zwc ]]; then
    zcompile $zdump
fi

zstyle :compinstall filename "$ZDOTDIR/.zshrc"
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"

zle_highlight=(region:bg=blue,fg=white,bold)

if [[ -o interactive ]] && [[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]]; then
    typeset -g _SDKMAN_LOADED=0
    sdk() {
        if (( ! _SDKMAN_LOADED )); then
            unfunction sdk
            source "$SDKMAN_DIR/bin/sdkman-init.sh"
            _SDKMAN_LOADED=1
        fi
        sdk "$@"
    }
fi
command -v direnv &>/dev/null && eval "$(direnv hook zsh)"
command -v atuin &>/dev/null && eval "$(atuin init zsh)"
