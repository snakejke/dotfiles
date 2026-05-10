#!/bin/bash

INSTALL_DIR="/opt/zotero"
TMP_DIR="/tmp/zotero_install"

# Проверка прав
if [[ "$EUID" -ne 0 ]]; then
  echo "Ошибка: Запустите через sudo"
  exit 1
fi

echo "--- Проверка обновлений Zotero ---"

LATEST_VERSION=""
TAGS=$(curl -s "https://api.github.com/repos/zotero/zotero/tags?per_page=10" | jq -r '.[].name')

for TAG in $TAGS; do
    URL="https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64&version=${TAG}"
    STATUS=$(curl -sIL "$URL" | grep "^HTTP" | tail -1)
    if echo "$STATUS" | grep -q "200"; then
        LATEST_VERSION="$TAG"
        break
    fi
done

if [ -z "$LATEST_VERSION" ]; then
    echo "Ошибка: Не удалось найти доступную версию для скачивания."
    exit 1
fi

if [ -f "$INSTALL_DIR/app/application.ini" ]; then
    CURRENT_VERSION=$(grep '^Version=' "$INSTALL_DIR/app/application.ini" | cut -d'=' -f2 | tr -d '\r')
    echo "Текущая: $CURRENT_VERSION | Доступна: $LATEST_VERSION"
else
    CURRENT_VERSION="none"
    echo "Zotero не найден. Будет выполнена чистая установка версии $LATEST_VERSION."
fi

if [ "$LATEST_VERSION" == "$CURRENT_VERSION" ]; then
    echo "Обновление не требуется."
    exit 0
fi

DOWNLOAD_URL="https://download.zotero.org/client/release/${LATEST_VERSION}/Zotero-${LATEST_VERSION}_linux-x86_64.tar.xz"

mkdir -p "$TMP_DIR"
rm -rf "$TMP_DIR"/*

MANUAL_FILE="/tmp/Zotero-${LATEST_VERSION}_linux-x86_64.tar.xz"

if [ -f "$MANUAL_FILE" ]; then
    echo "Найден файл $MANUAL_FILE, использую его."
    cp "$MANUAL_FILE" "$TMP_DIR/zotero.tar.xz"
else
    echo "Скачивание $LATEST_VERSION..."
    if ! curl -L -o "$TMP_DIR/zotero.tar.xz" "$DOWNLOAD_URL"; then
        echo ""
        echo "Ошибка скачивания (возможно геоблокировка)."
        echo "Скачайте файл вручную в браузере:"
        echo "  https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64&version=${LATEST_VERSION}"
        echo "И сохраните его как: $MANUAL_FILE"
        echo "Затем запустите скрипт снова."
        exit 1
    fi
fi

FILE_SIZE=$(stat -c%s "$TMP_DIR/zotero.tar.xz")
if [ "$FILE_SIZE" -lt 10000000 ]; then
    echo "Ошибка: Скачанный файл подозрительно мал ($FILE_SIZE байт)."
    exit 1
fi

echo "Распаковка архива..."
if ! tar -xJf "$TMP_DIR/zotero.tar.xz" -C "$TMP_DIR"; then
    echo "Ошибка распаковки!"
    exit 1
fi

EXTRACTED_FOLDER=$(find "$TMP_DIR" -maxdepth 1 -type d -name "Zotero*" | head -n 1)

if [ -z "$EXTRACTED_FOLDER" ]; then
    echo "Ошибка: Не удалось найти распакованную папку."
    exit 1
fi

echo "Обновление файлов в $INSTALL_DIR..."
rm -rf "$INSTALL_DIR"
mv "$EXTRACTED_FOLDER" "$INSTALL_DIR"

echo "Финальная настройка..."
if [ -f "$INSTALL_DIR/set_launcher_icon" ]; then
    "$INSTALL_DIR/set_launcher_icon"
fi

ln -sf "$INSTALL_DIR/zotero.desktop" "/usr/share/applications/zotero.desktop"
ln -sf "$INSTALL_DIR/zotero" "/usr/local/bin/zotero"

sed -i "s|Icon=.*|Icon=$INSTALL_DIR/chrome/icons/default/default256.png|g" "/usr/share/applications/zotero.desktop"

rm -rf "$TMP_DIR"
echo "Успешно обновлено до версии $LATEST_VERSION!"
