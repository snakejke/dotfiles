{ config, lib, pkgs, ... }:

let
  externalPackages = pkgs.callPackage ./external {};
in

{
  imports = [
    ./config/fdm.nix
    ./config/git.nix
    # ./config/devilspie2.nix # Оконный менеджмент в WSL не нужен
    ./config/zathura.nix
    ./config/dbeaver.nix
    ./config/wget.nix
    ./config/aria2.nix
    # ./config/dunst.nix # Уведомления можно оставить, но часто используют win-notify
    # ./config/rofi.nix # Можно включить, если планируешь запускать линуксовый лаунчер
    ./config/ssh.nix
    ./config/texlive.nix
    ./config/mise.nix
    ./config/readline.nix
    ./config/themes.nix
    ./config/atuin.nix
    # ./config/zsh.nix # Раскомментируй, если используешь, или настрой отдельно
  ];
    
  home.username = "snake";
  home.homeDirectory = "/home/snake";
  
  nixpkgs.config = import ./nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  home.stateVersion = "24.11"; 
  home.preferXdgDirectories = true;

  home.packages = with pkgs; [
    # --- System / Utils (Оставил только то, что полезно в WSL) ---
    ueberzugpp
    fd
    jq
    fzf
    ripgrep
    bat
    gdu
    lf
    ranger
    # xdotool # Обычно не работает в WSLg корректно
    # wmname
    vlc
    fastfetch
    qtpass
    # qbittorrent # Можно, но обычно торренты качают через Windows
    parallel
    # parted # Работа с дисками в WSL не нужна
    pandoc
    lnav
    neovim
    # hdparm # Железо виртуальное
    keepassxc
    fdupes
    # evtest
    ditaa
    # devilspie2
    maildrop
    lynx
    tree
    mermaid-cli
    zstd
    _7zz-rar
    zip
    pv
    unixtools.xxd
    # wmctrl
    yt-dlp
    cachix
    # sysfsutils
    # scrcpy # USB проброс в WSL сложен, проще через Win
    pgloader
    amazon-q-cli
    
    # --- Network ---
    # bridge-utils 
    tcpdump
    dig
    drill
    speedtest-cli
    nettools
    nmap
    mtr 
    # ndisc6 

    # --- Sync ---
    socat
    zsync
    # swtpm 
    restic
    rclone
    rsync

    # --- Images / Screenshot ---
    nsxiv
    # maim # Скриншоты лучше делать средствами Windows (Win+Shift+S)
    # slop
    # scrot
    # flameshot # Можно оставить, если нравится именно он
    feh
    pngquant
    (tesseract5.override {
      enableLanguages = [ "eng" "rus" ];
    })
    libsixel
    lsix

    # --- Documents ---
    poppler-utils
    exiftool
    mupdf-headless
    hexapdf
    ocrmypdf
    librsvg
    wkhtmltopdf
    ghostscript
    koodo-reader
    foliate
    djvulibre
    visidata

    # --- Dev / Languages ---
    nil
    ansible
    ansible-lint
    # k3d/kubectl в WSL работают, но требуют запущенного Docker Desktop
    (pkgs.k3d.override {
      k3sVersion = "1.30.14-k3s2";
    })
    kubectl
    k9s
    podman-tui
    podman-compose
    lazydocker
    kubernetes-helm
    skopeo
    exercism

    # --- GUI / Themes ---
    # lxappearance # Полезно для настройки темы приложений в WSLg
    adapta-gtk-theme
    mate.mate-themes
    papirus-icon-theme
    bibata-cursors
    capitaine-cursors
    greybird
    yaru-theme
    fluent-gtk-theme

    # --- Monitoring ---
    btop
    htop
    lsof
    strace

    zsh-z
    zsh-autosuggestions
    zsh-syntax-highlighting

    nvfetcher
    
    # --- Programming ---
    mitschemeX11
    guile
    racket
    fpc
    nim
    nimble
    nimlangserver
    
    # WSL Specific
    wslu # Утилиты для интеграции с Windows (открыть ссылку в браузере Win и т.д.)
  ] ++ externalPackages;

  # Фикс для буфера обмена в WSL (опционально, если не работает из коробки)
  # systemd в новых WSL работает, так что home-manager сервис запустится.

  home.file.".config/python/pythonrc".text = ''
    import readline
    readline.write_history_file = lambda *args: None
  '';
  
  xdg.configFile."npm/npmrc".text = ''
    prefix=''${XDG_DATA_HOME}/npm
    cache=''${XDG_CACHE_HOME}/npm
    init-module=''${XDG_CONFIG_HOME}/npm/config/npm-init.js
    logs-dir=''${XDG_STATE_HOME}/npm/logs
  '';

  xdg.configFile."pnpm/rc".text = ''
    auto-install-peers=true
    ignore-scripts=false
    dangerouslyAllowAllBuilds=true
  '';

  xdg.configFile."nimble/nimble.ini".text = ''
    nimbleDir = ~/.local/share/nimble
  '';
  
  # Настройка браузера по умолчанию через wslview (из пакета wslu)
  home.sessionVariables = {
    BROWSER = "wslview";
  };

  programs.home-manager.enable = true;
}
