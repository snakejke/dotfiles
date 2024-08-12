#!/bin/bash

# Директория назначения для резервных копий
backup_dir="/home/snake/Documents/SVALKA/ArchBACKUP/"

# Создаем директорию для резервных копий, если она не существует
mkdir -p "$backup_dir"

# Функция для выполнения резервного копирования с помощью rsync
backup_with_rsync() {
    source_path="$1"
    destination_path="$backup_dir${source_path#/}"
    exclude_patterns=("$2")  # Массив шаблонов для исключения (опционально)

    echo "Создание резервной копии $source_path -> $destination_path"

    if [ -d "$source_path" ]; then
        # Если исходный путь является директорией
        mkdir -p "$destination_path"
        if [ "${#exclude_patterns[@]}" -gt 0 ]; then
            rsync -au --info=progress2 --delete "${exclude_patterns[@]/#/--exclude=}" "$source_path/" "$destination_path/"
        else
            rsync -au --info=progress2 --delete "$source_path/" "$destination_path/"
        fi
    else
        # Если исходный путь является файлом
        mkdir -p "$(dirname "$destination_path")"
        if [ "${#exclude_patterns[@]}" -gt 0 ]; then
            rsync -au --info=progress2 --delete "${exclude_patterns[@]/#/--exclude=}" "$source_path" "$destination_path"
        else
            rsync -au --info=progress2 --delete "$source_path" "$destination_path"
        fi
    fi
}

# Функция для обработки массивов файлов/директорий для резервного копирования
backup_with_base_path() {
    base_path="$1"
    shift
    items=("$@")

    for item in "${items[@]}"; do
        backup_with_rsync "$base_path/$item"
    done
}

# Массивы файлов/директорий для резервного копирования
home_files=(
    "Downloads"
    "Documents"
    ".m2"
    ".local/mybuilds"
    ".local/devjava"
    ".local/bin"
    "Mail"
    "OrgFiles"
    ".zotero"
    "Zotero"
    ".password-store"
    ".gnupg"
    ".emacs.d"
)

config_files=(
    "alacritty"
    "btop"
    "chemacs"
    "com.github.johnfactotum.Foliate"
    "cwm"
    "doom"
    "dunst"
    "environment.d"
    "fdm"
    "flameshot"
    "git"
    "hypr"
    "i3"
    "ideavimrc"
    "lf"
    "libvirt"
    "lnav"
    "mpv"
    "nodejs"
    "notmuch"
    "npm"
    "nvidia-settings"
    "nvim"
    "nvm"
    "picom"
    "python"
    "ranger"
    "readline"
    "rofi"
    "systemd"
    "tmux"
    "vanilla"
    "wget"
    "xremap"
    "xsettingsd"
    "zathura"
    "zsh"
)

etc_files=(
    "dnscrypt-proxy"
    "logrotate.d"
    "mail"
    "modprobe.d"
    "pacman.d/hooks"
    "profile.d"
    "smtpd"
    "unbound"
    "environment"
    "fstab"
    "rsyslog.conf"
)



# Шаблоны для исключения файлов или директорий
exclude_home_patterns=("Documents/SVALKA")
#exclude_etc_patterns=("profile.d")

# Вызов функции для каждого базового пути с соответствующим массивом и шаблонами для исключения
#backup_with_rsync "/home/snake/.config" "${exclude_home_patterns[@]}"
#backup_with_base_path "/etc" "${etc_files[@]}" "${exclude_etc_patterns[@]}"
backup_with_base_path "/home/snake" "${home_files[@]}" "${exclude_home_patterns[@]}"
backup_with_base_path "/home/snake/.config" "${config_files[@]}"
backup_with_base_path "/etc" "${etc_files[@]}"

echo "Резервное копирование завершено."
