#!/bin/bash
set -e

echo "👉 Проверка необходимых пакетов: git, rsync, ansible..."

# Устанавливаем нужные пакеты, если не установлены
for pkg in git rsync ansible; do
  if ! command -v "$pkg" > /dev/null 2>&1; then
    echo "📦 Устанавливаю $pkg..."
    sudo xbps-install -Sy "$pkg"
  else
    echo "✅ $pkg уже установлен"
  fi
done

# Клонируем твой репозиторий, если нужно
REPO_URL="https://github.com/yourname/dotfiles.git"
CLONE_DIR="$HOME/.local/ansible_main"

if [ ! -d "$CLONE_DIR" ]; then
  echo "📥 Клонируем репозиторий $REPO_URL в $CLONE_DIR..."
  git clone "$REPO_URL" "$CLONE_DIR"
else
  echo "🔄 Репозиторий уже существует: $CLONE_DIR"
fi

cd "$CLONE_DIR"

# Запускаем установку ansible
echo "🚀 Запуск ansible-playbook install.yml..."
ansible-playbook -i hosts install.yml --ask-become-pass
