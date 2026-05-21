nix_source() {
  local plugin_path
  for profile in ${=NIX_PROFILES}; do
    plugin_path="$profile/share/$1"
    [[ -f "$plugin_path" ]] && { source "$plugin_path"; return 0 }
  done
  return 1
}

download() { aria2c -d ~/Downloads "$1"; }

nvm() {
    unset -f nvm  
    source "$NVM_DIR/nvm.sh"
    nvm "$@"
}

rm() {
    local RED=$'\033[0;31m'
    local NC=$'\033[0m'
    print "Do you want to ${RED}REMOVE${NC} this? (y/n)"
    command rm "$@" -i
}

function copy-line-to-clipboard() {
  echo -n "$BUFFER" | xclip -selection clipboard
  region_highlight=("0 $#BUFFER bg=blue,fg=white,bold")
  zle -R
  read -t 0.15 _ </dev/tty 2>/dev/null || true
  region_highlight=()
  zle -R
}

zle -N copy-line-to-clipboard

docx2emacs() {
    local tmpfile=$(mktemp --suffix=.txt)
    docx2txt "$1" > "$tmpfile"
    emacsclient -c -n "$tmpfile"
}

hm() {
    local switch_flag=false update_flag=false garbage_flag=false

    while [[ $# -gt 0 ]]; do
        case $1 in
            -s|--switch)
                switch_flag=true
                shift
                ;;
            -u|--update)
                update_flag=true
                shift
                ;;
            -d|--delete|--garbage)
                garbage_flag=true
                shift
                ;;
            *)
                echo "Неизвестная опция: $1"
                return 1
                ;;
        esac
    done

    if [[ $update_flag == true ]]; then
        nix flake update --flake ~/.config/home-manager || return
    fi

    if [[ $switch_flag == true ]]; then
        home-manager switch --flake ~/.config/home-manager || return
    fi

    if [[ $garbage_flag == true ]]; then
        nix-collect-garbage -d
    fi
}

untar() {
  local file="$1"
  if [[ -z "$file" ]]; then
    echo "Usage: untar <archive>"
    return 1
  fi

  if [[ ! -f "$file" ]]; then
    echo "Error: '$file' not found"
    return 1
  fi

  local size base mime
  size=$(stat -c %s "$file")
  base="${file##*/}"
  base="${base%.*}"

  case "$file" in
    *.tar.gz|*.tgz)   pv -s "$size" "$file" | tar xzf - ;;
    *.tar.bz2|*.tbz2) pv -s "$size" "$file" | tar xjf - ;;
    *.tar.xz|*.txz)   pv -s "$size" "$file" | tar xJf - ;;
    *.tar.zst)        pv -s "$size" "$file" | tar -I zstd -xf - ;;                  
    *.tar)            pv -s "$size" "$file" | tar xf - ;;
    *.gz)             pv -s "$size" "$file" | gunzip > "$base" ;;                  
    *.zip|*.rar|*.7z)
      mkdir -p "$base"
      7zz x "$file" -o"$base"
      ;;
    *)
      mime=$(file --brief --mime-type "$file")

      case "$mime" in
        application/zip|application/x-7z-compressed|application/x-rar)
          mkdir -p "$base"
          7zz x "$file" -o"$base"
          ;;
        *)
          echo "Unsupported format: $file (MIME: $mime)"
          ;;
      esac
      ;;
  esac
}

# pyt                  
# pyt my-temp-project  
# pyt my-app requests pandas 
pyt() {
    local project_name="${1:-python_$(date +%s)}"
    local project_dir="/tmp/${project_name}"

    mkdir -p "${project_dir}" && cd "${project_dir}" || return 1

    uv venv -q && source .venv/bin/activate || return 1

    if [[ $# -gt 1 ]]; then
        uv pip install "${@:2}"
    fi
}
