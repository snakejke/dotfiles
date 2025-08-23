{ config, lib, pkgs, ... }:

let
  externalPackages = pkgs.callPackage ./external {};
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
  home.preferXdgDirectories = true;

  home.packages = with pkgs; [
    ueberzugpp
    fd
    jq
    fzf
    ripgrep
    bat
    gdu
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
    drill
    qbittorrent
    podman-tui
    pngquant
    parallel
    parted
    pandoc
    papirus-icon-theme
    nsxiv
    maim
    slop
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
    curl
    mermaid-cli
    bridge-utils # brctl
    zstd 
    tcpdump

    #pdf tools
    poppler-utils
    mupdf-headless
    ocrmypdf
    librsvg # rsvg-convert
    wkhtmltopdf # rendering web pages to PDF or images
    ghostscript

    
    #lsp
    nil # nix 

    ansible
    ansible-lint
    #
    (pkgs.k3d.override {
      k3sVersion = "1.30.14-k3s2";
    })
    kubectl
    k9s

    foliate

    mate.mate-themes

    exercism
    koodo-reader
    
    #google-chrome ugly fonts 
    #discord need gpu accel 

    nettools

    (tesseract5.override {
      enableLanguages = [ "eng" "rus" ];
    })

    lxappearance
    
    #themes
    adapta-gtk-theme
    
    
    #p7zip # bin 7z
    _7zz-rar # 7zz 

    hexapdf # pdf cover 

    pv # Pipe Viewer
    
    #monitoring
    btop
    htop

    zsh-z
    zsh-autosuggestions

    nvfetcher

    mitschemeX11
    guile
    racket
    fpc #pascal 

  ] ++ externalPackages;

   programs.mise = {
    enable = true;
    
    globalConfig = {
      tools = {
        babashka = "latest";
        elixir = "latest";
        erlang = "latest";
        rebar = "latest";
      };
      tasks."update:hex" = {
      description = "Обновляет Hex и Rebar для Elixir";
      run = [
        "mix local.hex --force"
        "mix local.rebar --force"
      ];
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

  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        collection-basic
        collection-latex
        collection-fontsextra
        
        babel-russian
        hyphen-russian
        cyrillic
        lh
        cm-super
        luahyphenrules

        svg
        moderncv
        fontawesome
        
        geometry
        enumitem
        hyperref
        xcolor
        graphics
        fancyvrb
        
        dvisvgm
        dvipng
        
        tools
        oberdiek
        scheme-medium 
        titlesec

        wrapfig
        multirow
        ulem
        booktabs
        amsmath
        capt-of
        amsfonts;
    };
  };

  home.file.".config/python/pythonrc".text = ''
    import readline
    readline.write_history_file = lambda *args: None
  '';


  programs.readline = {
    enable = true;
    
    includeSystemConfig = true;
    
    variables = {
      editing-mode = "vi";
      show-mode-in-prompt = true;
      vi-ins-mode-string = "\\1\\e[6 q\\2";
      vi-cmd-mode-string = "\\1\\e[2 q\\2";
    };
    
    bindings = {
      "Control-l" = "clear-screen";
      "Control-a" = "beginning-of-line";
    };
    
    extraConfig = ''
      $if mode=vi
      set keymap vi-command
      # these are for vi-command mode  
      Control-l: clear-screen
      Control-a: beginning-of-line
      set keymap vi-insert
      # these are for vi-insert mode
      Control-l: clear-screen  
      Control-a: beginning-of-line
      $endif
    '';
  };

  xdg.configFile."readline/inputrc".text = config.xdg.configFile.inputrc.text;
  xdg.configFile.inputrc.enable = false;


  
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
