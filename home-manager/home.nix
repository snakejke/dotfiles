{ config, lib, pkgs, ... }:

let
  sources = pkgs.callPackage ./external/_sources/generated.nix {};
  
  palantir-cli = pkgs.callPackage ./external/packages/palantir-cli.nix {
    inherit sources;
  };
in

{
    imports = [
    ./config/fdm.nix
    ./config/git.nix
    ./config/devilspie2.nix
    ./config/zathura.nix
    ./config/dbeaver.nix
    ./config/wget.nix
    ./config/aria2.nix
    ./config/dunst.nix
    ./config/tmux.nix 
    ./config/rofi.nix
    # ./config/zsh.nix
    ];
    
  home.username = "snake";
  home.homeDirectory = "/home/snake";
  nixpkgs.config.allowUnfree = true;

  home.stateVersion = "24.11"; # Please read the comment before changing!!!

  home.packages = with pkgs; [
    ueberzugpp
    fd
    jq
    fzf
    ripgrep
    bat
    gdu
    firefox
    lf
    ranger
    xdotool
    wmname
    vlc
    strace
    speedtest-cli
    scrot
    rsync
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
    neovim
    # i3lock https://github.com/nix-community/home-manager/issues/7027
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
    curl 

    ansible
    ansible-lint

    foliate

    mate.mate-themes

    exercism
    koodo-reader
    
    #google-chrome
    #discord need gpu accel 

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

    zsh-z
    zsh-autosuggestions

    nvfetcher
    palantir-cli

  ];

   programs.mise = {
    enable = true;
    
    globalConfig = {
      tools = {
        babashka = "latest";
        elixir = "latest";
        erlang = "latest";
      };
    };
   };

    programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    daemon.enable = false;
    
    settings = {
      style = "compact";
      inline_height = 23;
      show_help = false;
    };
  };

  programs.ssh = {
    enable = true;
    package = null;
    
    addKeysToAgent = "yes";
    
    matchBlocks = {
      "github-gmail" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519_gmail";
        identitiesOnly = true;
      };
      
      "github.com" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519";
      };
    };
  };
  
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
