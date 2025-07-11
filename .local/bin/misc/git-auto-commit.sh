#!/bin/bash

# Включаем строгий режим
set -e

# Настройка логирования
exec 1> >(logger -s -t $(basename $0)) 2>&1

# Определение переменных окружения
export HOME=/home/snake  # Укажите правильный путь
# export PATH=/usr/local/bin:/usr/bin:/bin

# Пути
REPO_PATH="/home/snake/OrgFiles"
LOCK_FILE="/tmp/git_commit.lock"
TODAY_FILE="/tmp/git_commit_today"
LOG_FILE="/home/snake/.local/var/log/git_backup.log"

# Функция логирования
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" >> "$LOG_FILE"
}

# Проверка наличия необходимых команд
command -v git >/dev/null 2>&1 || { log_message "Git не установлен"; exit 1; }

# Проверка существования репозитория
if [ ! -d "$REPO_PATH/.git" ]; then
    log_message "Директория $REPO_PATH не является git репозиторием"
    exit 1
fi

# Проверка выполнения за сегодня
today=$(date +%Y-%m-%d)
if [ -f "$TODAY_FILE" ]; then
    saved_date=$(cat "$TODAY_FILE")
    if [ "$saved_date" = "$today" ]; then
        log_message "Скрипт уже выполнялся сегодня"
        exit 0
    fi
fi

# Создание lock-файла
if ! mkdir "$LOCK_FILE" 2>/dev/null; then
    log_message "Скрипт уже запущен (lock-файл существует)"
    exit 0
fi

# Удаление lock-файла при выходе
trap 'rm -rf "$LOCK_FILE"; log_message "Завершение работы скрипта"' EXIT

# Переход в директорию репозитория
cd "$REPO_PATH" || { log_message "Не удалось перейти в $REPO_PATH"; exit 1; }

# Получение изменений
if ! git fetch origin; then
    log_message "Ошибка при получении изменений из репозитория"
    exit 1
fi

# Добавление изменений
git add -A

# Проверка наличия изменений и создание коммита
if git diff --staged --quiet; then
    log_message "Изменений нет, создаем пустой коммит"
    echo "$(date)" > activity.txt
    git add activity.txt
    git commit -m "Добавлена отметка активности: $(date)"
else
    log_message "Найдены изменения, создаем коммит"
    git commit -m "Добавлено: $(date)"
fi

# Отправка изменений
if ! git push origin org; then
    log_message "Ошибка при отправке изменений"
    exit 1
fi

# Запись даты выполнения
echo "$today" > "$TODAY_FILE"
log_message "Скрипт успешно выполнен"
