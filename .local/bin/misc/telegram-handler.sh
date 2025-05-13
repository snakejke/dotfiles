#!/bin/bash

# Получаем переданный URL
input_url="$1"

# Функция для преобразования различных протоколов в https
convert_url() {
    local url="$1"
    local converted_url=""

    # Обработка инвайт-ссылок Telegram
    if [[ "$url" =~ t\.me/\+([A-Za-z0-9_-]+) ]]; then
        invite_code="${BASH_REMATCH[1]}"
        converted_url="https://web.telegram.org/k/#?tgaddr=tg://join?invite=$invite_code"
    # Обработка ссылок на каналы/юзеров
    elif [[ "$url" =~ tg://resolve\?domain=(.*) ]]; then
        username="${BASH_REMATCH[1]}"
        converted_url="https://web.telegram.org/a/#@$username"
    elif [[ "$url" =~ ^telegram\.me/(.*) ]] || [[ "$url" =~ ^t\.me/(.*) ]]; then
        username="${BASH_REMATCH[1]}"
        # Проверяем, не является ли это инвайт-ссылкой
        if [[ "$username" != +* ]]; then
            converted_url="https://web.telegram.org/a/#@$username"
        else
            # Если это инвайт-ссылка, передаем исходный URL
            converted_url="$url"
        fi
    else
        # Если формат не распознан, возвращаем исходный URL
        converted_url="$url"
    fi

    echo "$converted_url"
}

# Проверяем, что URL был передан
if [ -z "$input_url" ]; then
    echo "Ошибка: URL не указан"
    exit 1
fi

# Конвертируем URL
converted_url=$(convert_url "$input_url")

# Открываем в системном браузере по умолчанию
if command -v xdg-open >/dev/null 2>&1; then
    xdg-open "$converted_url"
else
    echo "Ошибка: xdg-open не установлен"
    exit 1
fi








# #!/bin/bash
# # Получаем переданный URL
# input_url="$1"

# if [[ -z "$input_url" ]]; then
#     echo "Ошибка: Не передана ссылка" >&2
#     exit 1
# fi

# # Обрабатываем ссылки Telegram
# if [[ "$input_url" =~ tg://resolve\?domain=([^&]+) ]]; then
#     username="${BASH_REMATCH[1]}"
#     clean_url="https://web.telegram.org/a/#@$username"  # Принудительно версия /a/
# elif [[ "$input_url" =~ tg://join\?invite=([^&]+) ]]; then
#     invite_code="${BASH_REMATCH[1]}"
#     clean_url="https://t.me/joinchat/$invite_code"
# elif [[ "$input_url" =~ tg://(user|msg|share|proxy|socks)\?(.*) ]]; then
#     clean_url="https://t.me/${BASH_REMATCH[1]}?${BASH_REMATCH[2]}"
# else
#     echo "Ошибка: Неподдерживаемый формат ссылки: $input_url" >&2
#     exit 1
# fi


# #!/bin/bash
# # Получаем переданный URL
# input_url="$1"

# if [[ -z "$input_url" ]]; then
#     echo "Ошибка: Не передана ссылка" >&2
#     exit 1
# fi

# # Обрабатываем разные типы ссылок tg://
# if [[ "$input_url" =~ tg://resolve\?domain=([^&]+) ]]; then
#     username="${BASH_REMATCH[1]}"
#     clean_url="https://web.telegram.org/a/#@$username"
# elif [[ "$input_url" =~ tg://join\?invite=([^&]+) ]]; then
#     invite_code="${BASH_REMATCH[1]}"
#     clean_url="https://t.me/joinchat/$invite_code"
# elif [[ "$input_url" =~ tg://(user|msg|share|proxy|socks)\?(.*) ]]; then
#     # Оставляем обработку для других типов ссылок, например, tg://msg?text=...
#     clean_url="https://t.me/${BASH_REMATCH[1]}?${BASH_REMATCH[2]}"
# else
#     echo "Ошибка: Неподдерживаемый формат ссылки: $input_url" >&2
#     exit 1
# fi


# !/bin/bash
# Получаем переданный URL
# input_url="$1"

# # Преобразуем tg://resolve?domain=nnmidlets в https://web.telegram.org/a/#@nnmidlets
# if [[ "$input_url" =~ tg://resolve\?domain=(.*) ]]; then
#     username="${BASH_REMATCH[1]}"
#     clean_url="https://web.telegram.org/a/#@$username"
#     google-chrome-stable --password-store=gnome-libsecret https://web.telegram.org/a/#@nnmidlets
# else
#     echo "Неверный формат ссылки: $input_url"
# fi

    # google-chrome-stable --password-store=gnome-libsecret https://web.telegram.org/a/#@nnmidlets
