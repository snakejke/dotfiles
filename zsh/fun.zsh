nix_source() {
  local plugin_path
  for profile in ${=NIX_PROFILES}; do
    plugin_path="$profile/share/$1"
    [[ -f "$plugin_path" ]] && { source "$plugin_path"; return 0 }
  done
  return 1
}

hm() {
    local switch_flag=false
    local update_flag=false
    local garbage_flag=false
    
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
            -h|--help)
                echo "Использование: hm [опции]"
                echo "  -s, --switch    Выполнить home-manager switch"
                echo "  -u, --update    Обновить flake"
                echo "  -d, --garbage   Очистить garbage collection"
                echo "  -h, --help      Показать справку"
                echo ""
                echo "Примеры:"
                echo "  hm -s           # только switch"
                echo "  hm -u -s        # update + switch"
                echo "  hm -u -s -d     # все команды"
                return 0
                ;;
            *)
                echo "Неизвестная опция: $1"
                echo "Используйте hm -h для справки"
                return 1
                ;;
        esac
    done
    
    if [[ $switch_flag == false && $update_flag == false && $garbage_flag == false ]]; then
        hm --help
        return 0
    fi
    
    if [[ $update_flag == true ]]; then
        nix flake update --flake ~/.config/home-manager
        if [[ $? -ne 0 ]]; then
            return 1
        fi
    fi
    
    if [[ $switch_flag == true ]]; then
        home-manager switch --flake ~/.config/home-manager
        if [[ $? -ne 0 ]]; then
            return 1
        fi
    fi
    
    if [[ $garbage_flag == true ]]; then
        nix-collect-garbage -d
        if [[ $? -ne 0 ]]; then
            return 1
        fi
    fi
}

untar() {
  local file="$1"
  if [[ -z "$file" ]]; then
    echo "Usage: untar <archive.tar.*>"
    return 1
  fi

  local size=$(stat -c %s "$file")

  case "$file" in
    *.tar.gz|*.tgz) pv -s "$size" "$file" | tar xzf - ;;
    *.tar.bz2|*.tbz2) pv -s "$size" "$file" | tar xjf - ;;
    *.tar.xz|*.txz) pv -s "$size" "$file" | tar xJf - ;;
    *.tar) pv -s "$size" "$file" | tar xf - ;;
    *) echo "Unsupported archive format: $file" ;;
  esac
}
