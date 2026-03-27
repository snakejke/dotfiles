{
  config,
  lib,
  pkgs,
  ...
}:

let
  externalPackages = pkgs.callPackage ./external { };
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
    #./config/tmux.nix
    ./config/rofi.nix
    ./config/ssh.nix
    ./config/texlive.nix
    #./config/emacs.nix
    ./config/mise.nix
    ./config/readline.nix
    ./config/themes.nix
    ./config/atuin.nix
    ./config/aria2.nix
    ./config/desktop-entries.nix
    ./config/podman.nix
    ./config/files.nix
    # ./config/zsh.nix
    # ./config/gpu-apps.nix
  ];

  home.username = "snake";
  home.homeDirectory = "/home/snake";
  nixpkgs.config = import ./nixpkgs-config.nix; # FIXME
  xdg.configFile."nixpkgs/config.nix".source = ./nixpkgs-config.nix;

  home.stateVersion = "25.11"; # Please read the comment before changing!!!
  home.preferXdgDirectories = true;

  targets.genericLinux.gpu = {
    enable = true;
    nvidia = {
      enable = true;
      version = "580.105.08";
      sha256 = "sha256-2cboGIZy8+t03QTPpp3VhHn6HQFiyMKMjRdiV2MpNHU=";
    };
  };

  home.packages =
    with pkgs;
    [
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
      fastfetch
      qtpass
      qbittorrent
      parallel
      parted
      pandoc
      lnav
      neovim
      # i3lock https://github.com/nix-community/home-manager/issues/7027
      hdparm
      keepassxc
      fdupes
      evtest
      ditaa
      delta
      devilspie2
      maildrop
      lynx
      tree
      mermaid-cli
      zstd
      _7zz-rar # 7zz
      zip
      pv # Pipe Viewer
      unixtools.xxd
      wmctrl
      yt-dlp
      cachix
      sysfsutils # systool
      scrcpy
      #pgloader #bug 04.02.2026
      firefox
      # discord 
      zed-editor
      notmuch
      notmuch.emacs

      #
      duckdb

      # gst_all_1.gstreamer
      # gst_all_1.gst-plugins-ugly
      # gst_all_1.gst-plugins-good
      # gst_all_1.gst-plugins-base
      # gst_all_1.gst-plugins-bad

      #
      bridge-utils # brctl
      tcpdump
      dig
      #curl
      drill
      speedtest-cli
      nettools
      nmap # ncat, gnu netcat nc, openbsd nc
      mtr # ping + traceroute
      ndisc6

      #
      socat
      zsync
      swtpm

      #
      restic
      rclone
      rsync

      #
      nsxiv
      maim
      slop
      scrot
      flameshot
      feh
      pngquant
      (tesseract5.override {
        enableLanguages = [
          "eng"
          "rus"
        ];
      })
      libsixel
      lsix

      #
      poppler-utils
      exiftool # alt pdfinfo
      mupdf-headless
      hexapdf # pdf cover
      # ocrmypdf <2026-02-26 Thu> Bug
      librsvg # rsvg-convert
      wkhtmltopdf # rendering web pages to PDF or images
      ghostscript
      koodo-reader # electron bug
      foliate
      djvulibre # ddjvu
      visidata
      convmv

      #
      ansible
      ansible-lint
      k3d
      # (pkgs.k3d.override {
      #   k3sVersion = "1.30.14-k3s2";
      # })
      kubectl
      k9s
      lazydocker
      kubernetes-helm
      skopeo

      #
      exercism

      #google-chrome ugly fonts

      #
      lxappearance
      adapta-gtk-theme
      mate-themes
      papirus-icon-theme
      bibata-cursors
      capitaine-cursors
      greybird
      yaru-theme
      fluent-gtk-theme

      #
      btop
      htop
      lsof
      strace

      zsh-z
      zsh-autosuggestions
      zsh-syntax-highlighting

      nvfetcher

      mitschemeX11
      guile
      racket
      fpc # pascal
      nim
      nimble
      nil # lsp nix
      nixfmt
      yq

      zeromq # libzmq.so jupyternim
      nimlangserver
      html-tidy

      # (python311.withPackages (ps: with ps; [
      #   jupyterlab
      #   ipython
      #   ipykernel

      #   numpy
      #   pandas
      #   matplotlib
      #   seaborn
      #   scipy

      #   requests
      #   beautifulsoup4
      # ]))
      jupyter-all

      devenv
      qrencode # QR generate
      zbar # QR reader
      # haskellPackages.greenclip
      # amnezia-vpn
      bubblewrap

    ]
    ++ externalPackages;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [
      exts.pass-otp
      exts.pass-audit
    ]);
    settings = { };
  };


  nix = {
    package = pkgs.nix;
    settings = {
      # fallback = true;
      # stalled-download-timeout = 10;
      # connect-timeout = 4;
      
      substituters = [
        # "https://cache.nixos.kz"
        # "https://mirror.yandex.ru/nixos"
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://cachix.cachix.org"
        "https://devenv.cachix.org"
      ];

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      ];
    };
  };



  home.sessionVariables = {
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
