#!/bin/bash
# Основные настройки
readonly USER_HOME="/home/snake"  # Базовый путь для домашней директории
readonly RCLONE_REPO="mailru:restic-repo"  # Путь в облаке Mail.ru
readonly REPO_PATH="rclone:$RCLONE_REPO"
readonly PASSWORD_FILE="$USER_HOME/.password-store/.restic"
readonly LOG_FILE="$USER_HOME/.local/var/log/restic_cloud_backup.log"
readonly LOCK_FILE="/tmp/restic_cloud_backup.lock"
readonly KEEP_LAST=2  # Количество сохраняемых бэкапов
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

# Проверка наличия необходимых программ
for cmd in restic rclone; do
    if ! command -v $cmd &> /dev/null; then
        echo "Error: $cmd не установлен" >&2
        exit 1
    fi
done

# Проверка настройки rclone
if ! rclone lsd mailru: &>/dev/null; then
    echo "Error: rclone не настроен для работы с Mail.ru Cloud" >&2
    echo "Настройте rclone используя: rclone config" >&2
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
    echo "Проверка целостности облачного репозитория..."
    if ! restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" check; then
        echo "Error: репозиторий поврежден" >&2
        exit 1
    fi
}

# Печать статистики
print_stats() {
    echo "Статистика облачного бэкапа:"
    restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" stats latest
}

# Проверка и инициализация репозитория
if ! restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" snapshots &>/dev/null; then
    echo "Инициализация облачного репозитория Restic..."
    if ! restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" init; then
        echo "Error: при инициализации репозитория" >&2
        exit 1
    fi
fi

home_files=(
    ".gnupg"
    ".password-store"
    ".ssh"
    "Documents"
    "Downloads"
    "Mail"
    "OrgFiles"
)

config_files=(
)

etc_files=(
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

echo "$(date '+%Y-%m-%d %H:%M:%S') Начало облачного резервного копирования"

# Создаем один снепшот со всеми путями
if [ ${#backup_paths[@]} -gt 0 ]; then
    echo "Создание облачной резервной копии следующих путей:"
    printf '%s\n' "${backup_paths[@]}"

    #             --limit-upload 1024 \
    # Добавляем ограничение скорости для облака
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

echo "$(date '+%Y-%m-%d %H:%M:%S') Облачное резервное копирование завершено"
