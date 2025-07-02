#!/bin/bash
# Основные настройки
readonly USER_HOME="/home/snake"  # Базовый путь для домашней директории
#readonly GOMAXPROCS=1
readonly RESTIC_COMPRESSION=off
readonly REPO_PATH="/mnt/spcc/restic-repo"
readonly PASSWORD_FILE="$USER_HOME/.password-store/.restic"
readonly LOG_FILE="$USER_HOME/.local/var/log/restic_backup.log"
readonly LOCK_FILE="/tmp/restic_backup.lock"
#7
readonly KEEP_LAST=3  # Количество сохраняемых бэкапов
readonly EXCLUDE_FILE="$USER_HOME/.local/bin/backup/restic-ignore.txt"

# Опции (инициализация до настройки логирования)
DRY_RUN=""
VERBOSE=""
INTERACTIVE=false

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --dry-run) DRY_RUN="--dry-run"; echo "Запущено в режиме dry-run";;
        --verbose|-v) VERBOSE="-v"; echo "Включено подробное логирование";;
        -vv) VERBOSE="-vv"; echo "Включено максимальное подробное логирование";;
        -vvv) VERBOSE="-vvv"; echo "Включено супер-подробное логирование";;
        --interactive|-i) INTERACTIVE=true; echo "Включен интерактивный режим";;
        *) echo "Неизвестный аргумент: $1" >&2; exit 1;;
    esac
    shift
done

# Настройка логирования в зависимости от режима
if [ "$INTERACTIVE" = false ]; then
    # В неинтерактивном режиме перенаправляем в лог
    exec >> "$LOG_FILE" 2>&1
else
    # В интерактивном режиме дублируем вывод
    exec > >(tee -a "$LOG_FILE") 2>&1
fi

# Проверка наличия restic
if ! command -v restic &> /dev/null; then
    echo "Error: restic не установлен" >&2
    exit 1
fi

# Блокировка для предотвращения параллельного запуска
if ! mkdir "$LOCK_FILE" 2>/dev/null; then
    echo "Error: Другой процесс резервного копирования уже запущен" >&2
    exit 1
fi

# Очистка блокировки при завершении
trap 'rm -rf "$LOCK_FILE"' EXIT

# Проверка репозитория
check_repository() {
    echo "Проверка целостности репозитория..."
    if ! restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" check; then
        echo "Error: репозиторий повреждён" >&2
        exit 1
    fi
}

# Печать статистики
print_stats() {
    echo "Статистика бэкапа:"
    restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" stats latest
}

# Проверка и инициализация репозитория
if ! restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" snapshots &>/dev/null; then
    echo "Инициализация репозитория Restic..."
    if ! restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" init; then
        echo "Error: при инициализации репозитория" >&2
        exit 1
    fi
fi

# Периодическая проверка репозитория (раз в неделю)
if [ -f "$REPO_PATH/last_check" ]; then
    last_check=$(cat "$REPO_PATH/last_check")
    now=$(date +%s)
    if [ $((now - last_check)) -gt 604800 ]; then  # 7 дней
        check_repository
        date +%s > "$REPO_PATH/last_check"
    fi
else
    check_repository
    date +%s > "$REPO_PATH/last_check"
fi

# Массивы файлов/директорий для резервного копирования
# home_files=(
#     ".gnupg"
#     ".local"
#     ".password-store"
#     ".ssh"
#     "Documents"
#     "Downloads"
#     "Mail"
#     "OrgFiles"
# )

config_files=(
    "ansible"
    "com.github.johnfactotum.Foliate"
    "containers"
    "devilspie2"
    "dunst"
    "emacs"
    "fdm"
    "flameshot"
    "fontconfig"
    "fvwm"
    "git"
    "gtk-2.0"
    "gtk-3.0"
    "gtk-4.0"
    "IJHack"
    "lf"
    "mpv"
    "notmuch"
    "nvim"
    "picom"
    "pipewire"
    "python"
    "ranger"
    "rclone"
    "readline"
    "rofi"
    "sx"
    "Thunar"
    "tmux"
    "xdg-desktop-portal"
    "xfce4"
    "xsettingsd"
    "zathura"
    "zsh"
    "mimeapps.list"
    "user-dirs.dirs"
    "user-dirs.locale"
)

etc_files=(
    "default/grub"
    "dnscrypt-proxy"
    "dracut.conf.d"
    "iptables"
    "profile.d/fixdbus.sh"
    "smtpd"
    "sudoers.d"
    "sv/agetty-autologin-tty1"
    "sv/amneziavpn"
    "unbound"
    "X11"
    "xbps.d"
    "dhcpcd.conf"
    "environment"
    "resolv.conf"
    "rsyslog.conf"
    "sysctl.conf"
)

# Формируем массив всех путей для бэкапа
backup_paths=()

# Добавляем пути из home_files
for file in "${home_files[@]}"; do
    if [ -e "$USER_HOME/$file" ]; then
        backup_paths+=("$USER_HOME/$file")
    else
        echo "Warning: $USER_HOME/$file не существует, пропускаем" >&2
    fi
done

# Добавляем пути из config_files
for file in "${config_files[@]}"; do
    if [ -e "$USER_HOME/.config/$file" ]; then
        backup_paths+=("$USER_HOME/.config/$file")
    else
        echo "Warning: $USER_HOME/.config/$file не существует, пропускаем" >&2
    fi
done

# Добавляем пути из etc_files
for file in "${etc_files[@]}"; do
    if [ -e "/etc/$file" ]; then
        backup_paths+=("/etc/$file")
    else
        echo "Warning: /etc/$file не существует, пропускаем" >&2
    fi
done

# Настройка исключений
if [ -f "$EXCLUDE_FILE" ]; then
    EXCLUDE_ARGS=("--exclude-file=$EXCLUDE_FILE")
else
    EXCLUDE_ARGS=("--exclude=Documents/SVALKA")
fi

echo "$(date '+%Y-%m-%d %H:%M:%S') Начало резервного копирования"

# Создаем один снепшот со всеми путями
if [ ${#backup_paths[@]} -gt 0 ]; then
    echo "Создание резервной копии следующих путей:"
    printf '%s\n' "${backup_paths[@]}"
    
    if ! restic -r "$REPO_PATH" \
         --password-file "$PASSWORD_FILE" \
         backup $DRY_RUN $VERBOSE "${backup_paths[@]}" \
         "${EXCLUDE_ARGS[@]}"; then
        echo "Error: ошибка при создании резервной копии" >&2
        exit 1
    fi
else
    echo "Error: нет существующих путей для резервного копирования" >&2
    exit 1
fi

# Очистка старых резервных копий
echo "Очистка старых резервных копий..."
if ! restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" forget --keep-last $KEEP_LAST --prune; then
    echo "Warning: ошибка при очистке старых копий" >&2
fi

# Вывод статистики
print_stats

echo "$(date '+%Y-%m-%d %H:%M:%S') Резервное копирование завершено"
