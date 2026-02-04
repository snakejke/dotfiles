# Развертывание конфига в Windows WSL (Ubuntu)

Эта инструкция поможет поднять твое окружение на свежеустановленной WSL Ubuntu.

## 1. Установка Nix

В WSL Ubuntu выполни команду для установки Nix в многопользовательском режиме (рекомендуется):

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

После установки **перезапусти терминал**.

## 2. Включение Flakes

Nix Flakes необходимы для работы данного конфига. Убедись, что они включены:

```bash
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

## 3. Клонирование конфигурации

Перейди в директорию конфигов (обычно это `~/.config/home-manager`):

```bash
mkdir -p ~/.config
# Склонируй свой репозиторий (замени URL на свой)
git clone https://github.com/youruser/home-manager-config.git ~/.config/home-manager
cd ~/.config/home-manager
```

## 4. Первый запуск Home Manager

Так как на свежей системе `home-manager` еще не установлен, используй `nix run` для инициализации профиля `snake@wsl`:

```bash
nix run home-manager/master -- switch --flake .#snake@wsl
```

## 5. Последующие обновления

После первого запуска команда `home-manager` станет доступна в системе. Для применения изменений используй:

```bash
home-manager switch --flake .#snake@wsl
```

## Особенности WSL профиля

1. **Интеграция с Windows**: Установлен пакет `wslu`. Теперь команды типа `wslview https://google.com` будут открывать ссылки в браузере Windows.
2. **GUI**: Если установлена Windows 11 или Windows 10 с поддержкой WSLg, графические приложения (VLC, NSXIV, и т.д.) будут работать "из коробки".
3. **Шрифты и Темы**: Темы GTK (`fluent-gtk-theme` и др.) применятся для линуксовых GUI приложений. Настройку можно произвести через `lxappearance`.
4. **Браузер**: Переменная `$BROWSER` настроена на `wslview`, чтобы `xdg-open` и другие утилиты использовали системный браузер Windows.

## Автоматическая настройка (рекомендуется)

Вместо ручного редактирования конфигов, вы можете использовать готовый PowerShell скрипт `setup-wsl.ps1`, который находится в корне этого репозитория.

1. Убедитесь, что вы создали пользователя `snake` при установке Ubuntu.
2. В Windows запустите **PowerShell** от имени администратора.
3. Перейдите в папку с конфигом (если она доступна из Windows) или скопируйте скрипт `setup-wsl.ps1` на рабочий стол Windows.
4. Запустите скрипт:
   ```powershell
   .\setup-wsl.ps1
   ```
   *Если скрипт не запускается из-за политики безопасности, выполните `Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass` перед запуском.*

Скрипт автоматически:
1. Пропишет `hostname = wsl` в конфиг дистрибутива.
2. Установит `snake` как дефолтного пользователя.
3. Перезапустит WSL.

После этого команда `home-manager switch --flake .` будет работать автоматически.
