{ config,lib,pkgs, ... }:

{
    imports = [
    ./config/fdm.nix
    ./config/git.nix
    ./config/devilspie2.nix
    ./config/zathura.nix
    ./config/dbeaver.nix
    ./config/wget.nix
    # ./config/zsh.nix
    ];
    
  home.username = "snake";
  home.homeDirectory = "/home/snake";
  nixpkgs.config.allowUnfree = true;

  home.stateVersion = "24.11"; # Please read the comment before changing!!!

  home.packages = with pkgs; [
    ueberzugpp
    # jdt-language-server
    fd
    jq
    ripgrep
    bat
    gdu
    firefox
    lf
    ranger
    xdotool
    wmname
    aria2
    vlc
    strace
    speedtest-cli
    scrot
    rsync
    rofi
    fastfetch
    restic
    rclone
    qtpass
    nyxt
    qbittorrent
    podman-tui
    pngquant
    parallel
    parted
    pandoc
    papirus-icon-theme
    nsxiv
    maim 
    lnav
    dunst
    neovim
    i3lock
    hdparm
    keepassxc
    flameshot
    fdupes
    feh 
    evtest
    ditaa
    devilspie2
    maildrop
    lynx
    dig
    tree
    nil 

    ansible
    ansible-lint

    foliate
    
    #google-chrome
    #discord need gpu accel 

    # xfce.thunar
    # xfce.thunar-volman
    # xfce.thunar-archive-plugin
    
    nettools

    (tesseract5.override {
      enableLanguages = [ "eng" "rus" ];
    })

    lxappearance
    
    #themes
    adapta-gtk-theme
    
    
    #mail
    #fdm
    
    p7zip # bin 7z
    
    #monitoring
    btop
    htop

  ];
  
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
