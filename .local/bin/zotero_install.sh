#!/bin/bash

# Указываем последнюю версию Zotero для автоматического обновления в будущем
ZOTERO_VERSION="7.0.3"
DOWNLOAD_URL="https://download.zotero.org/client/release/$ZOTERO_VERSION/Zotero-${ZOTERO_VERSION}_linux-x86_64.tar.bz2"
TMP_DIR="/tmp/zotero_install"
INSTALL_DIR="/opt/zotero"

# Проверка прав суперпользователя
if [[ "$EUID" -ne 0 ]]; then
  echo "Пожалуйста, запустите скрипт с правами суперпользователя (sudo)."
  exit 1
fi

# Создаем временную директорию
mkdir -p $TMP_DIR

# Скачиваем архив
echo "Скачивание Zotero..."
wget -O "$TMP_DIR/zotero.tar.bz2" $DOWNLOAD_URL

# Распаковываем архив в временную директорию
echo "Распаковка Zotero..."
tar -xjf "$TMP_DIR/zotero.tar.bz2" -C $TMP_DIR

# Копируем содержимое во /opt/zotero
echo "Установка Zotero в /opt/zotero..."
rm -rf $INSTALL_DIR # Удаляем предыдущую установку, если она была
mv "$TMP_DIR/Zotero_linux-x86_64" $INSTALL_DIR

# Запускаем скрипт для установки иконок
echo "Установка иконок Zotero..."
$INSTALL_DIR/set_launcher_icon

# Создаем символическую ссылку для удобного доступа к Zotero через меню приложений
echo "Создание символической ссылки для приложения Zotero..."
REAL_USER=$(logname)
REAL_HOME=$(eval echo ~$REAL_USER)
mkdir -p $REAL_HOME/.local/share/applications
ln -s $INSTALL_DIR/zotero.desktop $REAL_HOME/.local/share/applications/zotero.desktop
chown $REAL_USER:$REAL_USER $REAL_HOME/.local/share/applications/zotero.desktop

# Очистка временных файлов
echo "Очистка временной директории..."
rm -rf $TMP_DIR

echo "Установка Zotero завершена."
