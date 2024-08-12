#!/bin/bash

# Проверка привилегий
check_privileges() {
    if [[ $EUID -ne 0 ]]; then
        echo "Этот скрипт должен быть запущен с правами суперпользователя."
        exit 1
    fi
}

# Цвета для вывода
green='\033[0;32m'
red='\033[0;31m'
nc='\033[0m'

# Функция для вывода сообщений
print_msg() {
    echo -e "${green}$1${nc}"
}

# Функция для вывода ошибок
print_error() {
    echo -e "${red}$1${nc}"
}

# Проверка наличия btrfs-progs
check_btrfs_progs() {
    if ! command -v btrfs &> /dev/null; then
        print_error "Пакет btrfs-progs не установлен. Установите его перед продолжением."
        exit 1
    fi
}

# Получение информации о дисках
get_disk_info() {
    print_msg "Доступные диски:"
    lsblk -o name,size,type,mountpoint | grep -v "rom\|loop"
    read -p "Введите имя диска (например, vda): " diskname
    disk="/dev/$diskname"
}

# Разметка диска с использованием fdisk
partition_disk() {
    print_msg "Разметка диска $disk..."
    (
    echo g # Создать новую таблицу разделов GPT
    echo n # Новый раздел
    echo 1 # Номер раздела 1
    echo   # Первый сектор (по умолчанию)
    echo +512M # Последний сектор (+512M для EFI)
    echo t # Изменить тип раздела
    echo 1 # Тип раздела EFI
    echo n # Новый раздел
    echo 2 # Номер раздела 2
    echo   # Первый сектор (по умолчанию)
    echo   # Последний сектор (по умолчанию, использовать оставшееся пространство)
    echo w # Записать изменения и выйти
    ) | fdisk "$disk"
}

# Форматирование разделов
format_partitions() {
    print_msg "Форматирование разделов..."
    mkfs.vfat "${disk}1"
    mkfs.btrfs "${disk}2"
}

# Монтирование и создание subvolumes
mount_and_create_subvolumes() {
    print_msg "Монтирование корневого subvolume..."
    mount "${disk}2" /mnt

    print_msg "Создание дополнительных subvolumes..."
    for subvol in @ @home @log @xbps @postgres @mysql @docker @.snapshots; do
        btrfs subvolume create /mnt/$subvol
    done

    umount /mnt

    mount_opts="compress=zstd:3,ssd,discard=async,space_cache=v2"

    print_msg "Монтирование корневого subvolume с subvol=@..."
    mount -o $mount_opts,subvol=@ "${disk}2" /mnt

    print_msg "Монтирование дополнительных subvolumes..."
    mkdir -p /mnt/{home,var/log,var/cache/xbps,var/lib/postgresql16,var/lib/mysql,var/lib/docker,.snapshots}
    mount -o $mount_opts,subvol=@home "${disk}2" /mnt/home
    mount -o $mount_opts,subvol=@log "${disk}2" /mnt/var/log
    mount -o $mount_opts,subvol=@xbps "${disk}2" /mnt/var/cache/xbps
    mount -o noatime,nobarrier,$mount_opts,subvol=@postgres "${disk}2" /mnt/var/lib/postgresql16
    mount -o noatime,nobarrier,$mount_opts,subvol=@mysql "${disk}2" /mnt/var/lib/mysql
    mount -o noatime,$mount_opts,subvol=@docker "${disk}2" /mnt/var/lib/docker
    mount -o $mount_opts,subvol=@.snapshots "${disk}2" /mnt/.snapshots
}

# Монтирование раздела /boot
mount_boot() {
    print_msg "Монтирование раздела /boot..."
    mkdir -p /mnt/boot/efi
    mount "${disk}1" /mnt/boot/efi
}

# Монтирование основного btrfs пула
mount_btrfs_pool() {
    print_msg "Монтирование основного btrfs пула..."
    mkdir -p /mnt/mnt/btr_pool
    mount -o $mount_opts,subvolid=5 "${disk}2" /mnt/mnt/btr_pool
}

# Функция для выполнения команд в chroot окружении
run_in_chroot() {
    chroot /mnt /bin/bash -c "$1"
}

# Установка Void Linux
install_void_linux() {
    repo=https://repo-default.voidlinux.org/current
    arch=x86_64
    mkdir -p /mnt/var/db/xbps/keys
    cp /var/db/xbps/keys/* /mnt/var/db/xbps/keys/ || print_error "Ошибка копирования ключей"
    XBPS_ARCH=$arch xbps-install -Sy -r /mnt -R "$repo" base-system
}

# Монтирование системных каталогов
mount_system_dirs() {
    for dir in dev proc sys run; do
        mount --rbind /$dir /mnt/$dir
        mount --make-rslave /mnt/$dir
    done
}

# Копирование DNS настроек
copy_dns_settings() {
    cp /etc/resolv.conf /mnt/etc/
}

pause_for_confirmation() {
    read -p "Press Enter to continue after the $1 step or Ctrl+C to cancel..." dummy
}

# Первый этап внутри chroot
chroot_stage1() {
  run_in_chroot '
    echo "Первый этап внутри chroot"
    xbps-install -Sy void-repo-nonfree void-repo-multilib void-repo-multilib-nonfree
    echo "en_US.UTF-8 UTF-8" >> /etc/default/libc-locales
    echo "ru_RU.UTF-8 UTF-8" >> /etc/default/libc-locales
    xbps-reconfigure -f glibc-locales
    echo "LANG=en_US.UTF-8" >> /etc/locale.conf
    echo "LC_ALL=C.UTF-8" >> /etc/locale.conf
    ln -sf /usr/share/zoneinfo/Europe/Moscow /etc/localtime
    echo "KEYMAP=ruwin_alt_sh-UTF-8" > /etc/rc.conf
    echo "void" > /etc/hostname
  '  
}

# Второй этап внутри chroot
chroot_stage2() {
  run_in_chroot '
    cat > /etc/dracut.conf.d/boot.conf <<'DRACUT'
hostonly=yes
use_fstab=yes
compress="lz4"
omit_dracutmodules+=" i18n lvm lunmask dmraid mdraid "
kernel_cmdline="rd.skipfsck rd.luks=0 rd.lvm=0 rd.md=0 rd.dm=0"
add_drivers+=" nvidia nvidia-drm nvidia-modeset nvidia-uvm "
DRACUT
'
}

# Третий этап внутри chroot
chroot_stage3() {
  run_in_chroot '
    uuid=$(blkid -o value -s UUID /dev/vda2)
    boot_uuid=$(blkid -o value -s UUID /dev/vda1)
    cat > /etc/fstab <<'FSTAB'
UUID=$uuid / btrfs noatime,compress=zstd:3,ssd,discard=async,space_cache=v2,subvol=@     0 0
UUID=$uuid /home btrfs noatime,nosuid,nodev,compress=zstd:3,ssd,discard=async,space_cache=v2,subvol=@home  0 0
UUID=$uuid /var/log btrfs noatime,compress=zstd:3,ssd,discard=async,space_cache=v2,subvol=@log   0 0
UUID=$uuid /var/cache/xbps btrfs noatime,compress=zstd:3,ssd,discard=async,space_cache=v2,subvol=@xbps 0 0
UUID=$uuid /var/lib/postgresql16 btrfs noatime,nobarrier,compress=zstd:3,ssd,discard=async,space_cache=v2,subvol=@postgres 0 0
UUID=$uuid /var/lib/mysql btrfs noatime,nobarrier,compress=zstd:3,ssd,discard=async,space_cache=v2,subvol=@mysql 0 0
UUID=$uuid /var/lib/docker btrfs noatime,compress=zstd:3,ssd,discard=async,space_cache=v2,subvol=@docker 0 0
UUID=$uuid /.snapshots btrfs noatime,nodev,nosuid,noexec,compress=zstd:3,ssd,discard=async,space_cache=v2,subvol=@.snapshots 0 0
UUID=$uuid /mnt/btr_pool btrfs noatime,compress=zstd:3,ssd,discard=async,space_cache=v2,subvolid=5 0 0
UUID=$boot_uuid /boot/efi vfat defaults 0 2

proc /proc proc nosuid,nodev,noexec,hidepid=2,gid=proc 0 0
tmpfs /tmp tmpfs nosuid,nodev,noexec,strictatime,mode=1777 0 0
FSTAB


# Изменение пароля для root
echo "Изменение пароля для root..."
passwd root

# Создание нового пользователя
read -p "Enter name new user: " newuser
useradd -m -G wheel,floppy,audio,cdrom,optical,kvm,input,xbuilder -s /bin/bash "$newuser"

# Изменение пароля для нового пользователя
echo "Change password for $newuser..."
passwd "$newuser"

#wheel 
echo "%wheel ALL=(ALL:ALL) ALL" >/etc/sudoers.d/wheel-can-sudo

xbps-install -Sy grub-x86_64-efi refind
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id="Void"

refind-install

'
}

chroot_stage4_install_apps() {
  run_in_chroot '
  apps=(
  mariadb
  ImageMagick
  intel-ucode
  nvidia 
  nvidia-libs-32bit
  nvidia-vaapi-driver
  7zip
  base-devel
  bat 
  bind-utils
  kde5
  alacritty
  xorg
  rsyslog
  chrony
  sx 
  cmake
  aria2
  python3
  python3-pip
  python3-pipx
  cargo
  alacritty
  aspell
  aspell-en
  aspell-ru
  atool
  fd 
  fdm
  feh
  fzf
  gimp
  git 
  go 
  ranger
  btop
  lf
  ncdu
  neovim
  nodejs
  nerd-fonts
  neofetch
  docker
  docker-compose
  postgresql-full
  zathura
  zathura-cb
  zathura-djvu
  zathura-pdf-mupdf
  zathura-ps
  xtools
  xclip
  xsettingsd
  zsh
  zstd
  wget
  )

  xbps-install -Sy "${apps[@]}"

  ln -s /etc/sv/{dbus,chronyd,sddm,dhcpcd,rsyslogd} /var/service

  xbps-reconfigure -fa

'
}


# Основная функция
main() {
    check_privileges
    check_btrfs_progs
    get_disk_info
    partition_disk
    format_partitions
    mount_and_create_subvolumes
    mount_boot
    mount_btrfs_pool
    install_void_linux
    mount_system_dirs
    copy_dns_settings
    #pause_for_confirmation
    chroot_stage1
    #pause_for_confirmation
    chroot_stage2
    #pause_for_confirmation
    chroot_stage3
    chroot_stage4_install_apps
    #pause_for_confirmation
    umount -R /mnt
    shutdown -r now
}

# Запуск основной функции
main

