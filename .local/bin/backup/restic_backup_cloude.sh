#!/bin/bash

# ./script.sh -i → вывод дублируется в терминал, прогресс виден.
# ./script.sh -i --no-progress → вывод дублируется в терминал, прогресс отключён.
# ./script.sh (cron/job/systemd timer) → всё уходит в лог, прогресса нет.

# Основные настройки
readonly USER_HOME="/home/snake"
readonly RCLONE_REPO="mailru:restic-repo"
readonly REPO_PATH="rclone:$RCLONE_REPO"
readonly PASSWORD_FILE="$USER_HOME/.password-store/.restic"
readonly LOG_FILE="$USER_HOME/.local/var/log/restic_cloud_backup.log"
readonly LOCK_FILE="/tmp/restic_cloud_backup.lock"
readonly KEEP_LAST=3
readonly EXCLUDE_FILE="$USER_HOME/.local/bin/backup/restic-ignore.txt"

# Опции
DRY_RUN=""
VERBOSE=""
INTERACTIVE=false
NO_PROGRESS=false

# Обработка аргументов командной строки
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --dry-run) DRY_RUN="--dry-run"; echo "Запущено в режиме dry-run";;
        --verbose|-v) VERBOSE="-v"; echo "Включено подробное логирование";;
        -vv) VERBOSE="-vv"; echo "Включено максимальное подробное логирование";;
        -vvv) VERBOSE="-vvv"; echo "Включено супер-подробное логирование";;
        --interactive|-i) INTERACTIVE=true; echo "Включен интерактивный режим";;
        --no-progress) NO_PROGRESS=true; echo "Прогресс будет отключён";;
        *) echo "Неизвестный аргумент: $1" >&2; exit 1;;
    esac
    shift
done

# Настройка логирования
if $INTERACTIVE; then
    # В интерактиве вывод дублируем в терминал
    exec > >(tee -a "$LOG_FILE") 2>&1
else
    # В cron всё только в лог
    exec >> "$LOG_FILE" 2>&1
fi

# Настройка прогресса Restic
if $NO_PROGRESS; then
    export RESTIC_PROGRESS_FPS=0
else
    if $INTERACTIVE; then
        # принудительно включаем прогресс даже при tee
        export RESTIC_PROGRESS_FPS=0.5   # обновление 2 раза в секунду
    fi
    # в cron переменную не ставим → restic сам решит, прогресс не нужен
fi

# Функция для корректной обработки exit кодов restic
handle_restic_exit() {
    local exit_code=$1
    local operation_name=$2
    
    case $exit_code in
        0)
            echo "✓ $operation_name выполнена успешно"
            return 0
            ;;
        1)
            echo "✗ Критическая ошибка при $operation_name" >&2
            return 1
            ;;
        3)
            echo "⚠ $operation_name завершена с предупреждениями (некоторые файлы недоступны)" >&2
            return 0  # Продолжаем работу при предупреждениях
            ;;
        *)
            echo "✗ Неожиданная ошибка ($exit_code) при $operation_name" >&2
            return 1
            ;;
    esac
}

# Проверка зависимостей
check_dependencies() {
    local missing_deps=()
    
    for cmd in restic rclone; do
        if ! command -v "$cmd" &> /dev/null; then
            missing_deps+=("$cmd")
        fi
    done
    
    if [ ${#missing_deps[@]} -gt 0 ]; then
        echo "✗ Отсутствуют необходимые программы: ${missing_deps[*]}" >&2
        exit 1
    fi
    
    echo "✓ Все зависимости найдены"
}

# Проверка настройки rclone
check_rclone_config() {
    echo "Проверка настройки rclone..."
    if ! rclone lsd mailru: &>/dev/null; then
        echo "✗ rclone не настроен для работы с Mail.ru Cloud" >&2
        echo "Настройте rclone используя: rclone config" >&2
        exit 1
    fi
    echo "✓ rclone настроен корректно"
}

# Создание блокировки
create_lock() {
    if ! mkdir "$LOCK_FILE" 2>/dev/null; then
        echo "✗ Другой процесс резервного копирования уже запущен" >&2
        exit 1
    fi
    trap 'rm -rf "$LOCK_FILE"' EXIT
    echo "✓ Блокировка создана"
}

# Проверка репозитория
check_repository() {
    echo "Проверка целостности облачного репозитория..."
    restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" check
    handle_restic_exit $? "проверка репозитория" || exit 1
}

# Инициализация репозитория при необходимости
init_repository_if_needed() {
    echo "Проверка существования репозитория..."
    if ! restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" snapshots &>/dev/null; then
        echo "Инициализация облачного репозитория Restic..."
        restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" init
        handle_restic_exit $? "инициализация репозитория" || exit 1
    else
        echo "✓ Репозиторий уже существует"
    fi
}

# Подготовка путей для бэкапа
prepare_backup_paths() {
    local -a home_files=(
        ".gnupg"
        ".password-store"
        ".ssh"
        "Documents"
        "Downloads"
        "Mail"
        "OrgFiles"
    )
    
    local -a config_files=()
    local -a etc_files=()
    
    backup_paths=()
    
    # Добавляем существующие пути из home_files
    for file in "${home_files[@]}"; do
        if [ -e "$USER_HOME/$file" ]; then
            backup_paths+=("$USER_HOME/$file")
        else
            echo "⚠ $USER_HOME/$file не существует, пропускаем"
        fi
    done
    
    # Добавляем пути из config_files
    for file in "${config_files[@]}"; do
        if [ -e "$USER_HOME/.config/$file" ]; then
            backup_paths+=("$USER_HOME/.config/$file")
        else
            echo "⚠ $USER_HOME/.config/$file не существует, пропускаем"
        fi
    done
    
    # Добавляем пути из etc_files
    for file in "${etc_files[@]}"; do
        if [ -e "/etc/$file" ]; then
            backup_paths+=("/etc/$file")
        else
            echo "⚠ /etc/$file не существует, пропускаем"
        fi
    done
    
    if [ ${#backup_paths[@]} -eq 0 ]; then
        echo "✗ Нет существующих путей для резервного копирования" >&2
        exit 1
    fi
}

# Подготовка аргументов исключения
prepare_exclude_args() {
    if [ -f "$EXCLUDE_FILE" ]; then
        EXCLUDE_ARGS=("--exclude-file=$EXCLUDE_FILE")
        echo "✓ Используется файл исключений: $EXCLUDE_FILE"
    else
        EXCLUDE_ARGS=("--exclude=Documents/SVALKA")
        echo "✓ Используются стандартные исключения"
    fi
}

# Создание резервной копии
create_backup() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') Начало облачного резервного копирования"
    echo "Создание облачной резервной копии следующих путей:"
    printf '  %s\n' "${backup_paths[@]}"
    
    # Создаем бэкап с корректной обработкой ошибок
    restic -r "$REPO_PATH" \
        --password-file "$PASSWORD_FILE" \
        backup $DRY_RUN $VERBOSE "${backup_paths[@]}" \
        "${EXCLUDE_ARGS[@]}"
    
    local backup_exit_code=$?
    handle_restic_exit $backup_exit_code "создание резервной копии"
    
    # Возвращаем код только для критических ошибок
    if [ $backup_exit_code -eq 1 ]; then
        return 1
    fi
    
    return 0
}

# Очистка старых копий с автоматическим снятием stale lock
cleanup_old_backups() {
    echo "Очистка старых резервных копий..."

    # Попытка выполнить forget/prune
    restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" forget --keep-last $KEEP_LAST --prune
    local cleanup_exit_code=$?

    # Если ошибка 11 (repository is locked), пытаемся unlock и повторяем
    if [ $cleanup_exit_code -eq 11 ]; then
        echo "⚠ Репозиторий заблокирован, пытаемся снять stale lock..."
        restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" unlock || echo "✗ Не удалось снять lock"
        echo "Повторная попытка очистки старых копий..."
        restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" forget --keep-last $KEEP_LAST --prune
        cleanup_exit_code=$?
    fi

    # Обработка результатов
    if ! handle_restic_exit $cleanup_exit_code "очистка старых копий"; then
        echo "⚠ Продолжаем работу несмотря на проблемы с очисткой"
    fi
}

# Печать статистики
print_stats() {
    echo "Статистика облачного бэкапа:"
    restic -r "$REPO_PATH" --password-file "$PASSWORD_FILE" stats latest
    handle_restic_exit $? "получение статистики" || echo "⚠ Не удалось получить статистику"
}

# Основная функция
main() {
    echo "=== Запуск скрипта облачного резервного копирования ==="
    
    check_dependencies
    check_rclone_config
    create_lock
    init_repository_if_needed
    prepare_backup_paths
    prepare_exclude_args
    
    if create_backup; then
        cleanup_old_backups
        print_stats
        echo "$(date '+%Y-%m-%d %H:%M:%S') ✓ Облачное резервное копирование завершено успешно"
    else
        echo "$(date '+%Y-%m-%d %H:%M:%S') ✗ Облачное резервное копирование завершено с ошибкой"
        exit 1
    fi
}

# Запуск основной функции
main
