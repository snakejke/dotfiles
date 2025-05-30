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

. "$ZDOTDIR/aliases"

nix_source() {
  local plugin_path
  for profile in ${=NIX_PROFILES}; do
    plugin_path="$profile/share/$1"
    [[ -f "$plugin_path" ]] && { source "$plugin_path"; return 0 }
  done
  return 1
}

nix_source "zsh-autosuggestions/zsh-autosuggestions.zsh"
nix_source "zsh-z/zsh-z.plugin.zsh"
nix_source "fzf/key-bindings.zsh"

# TODO:
autoload -Uz +X compinit && compinit

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select

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

[[ -s "/home/snake/.local/devjava/sdkman/bin/sdkman-init.sh" ]] && source "/home/snake/.local/devjava/sdkman/bin/sdkman-init.sh"

eval "$(atuin init zsh)"
#zprof
