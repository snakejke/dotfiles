#!/bin/bash

set -e

exec 1> >(logger -s -t $(basename $0)) 2>&1

export HOME=/home/snake
export PATH=/usr/local/bin:/usr/bin:/bin:$HOME/.local/state/nix/profile/bin

REPO_PATH="/home/snake/OrgFiles"
LOCK_FILE="/tmp/git_commit.lock"
TODAY_FILE="/tmp/git_commit_today"
LOG_FILE="/home/snake/.local/var/log/git_backup.log"

log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$LOG_FILE"
}

command -v git >/dev/null 2>&1 || { log_message "Git не установлен"; exit 1; }

if [ ! -d "$REPO_PATH/.git" ]; then
    log_message "Директория $REPO_PATH не является git репозиторием"
    exit 1
fi

today=$(date +%Y-%m-%d)
if [ -f "$TODAY_FILE" ]; then
    saved_date=$(cat "$TODAY_FILE")
    if [ "$saved_date" = "$today" ]; then
        log_message "Скрипт уже выполнялся сегодня"
        exit 0
    fi
fi

if ! mkdir "$LOCK_FILE" 2>/dev/null; then
    log_message "Скрипт уже запущен (lock-файл существует)"
    exit 0
fi

trap 'rm -rf "$LOCK_FILE"; log_message "Завершение работы скрипта"' EXIT

cd "$REPO_PATH" || { log_message "Не удалось перейти в $REPO_PATH"; exit 1; }

if ! git fetch origin; then
    log_message "Ошибка при получении изменений из репозитория"
    exit 1
fi

git add -A

if git diff --staged --quiet; then
    log_message "Изменений нет, создаем пустой коммит"
    echo "$(date)" > activity.txt
    git add activity.txt
    git commit -m "Добавлена отметка активности: $(date)"
else
    log_message "Найдены изменения, создаем коммит"
    git commit -m "Добавлено: $(date)"
fi

if ! git push origin org; then
    log_message "Ошибка при отправке изменений"
    exit 1
fi

echo "$today" > "$TODAY_FILE"
log_message "Скрипт успешно выполнен"
