{ config, pkgs, ... }:
let
  gtk2ExtraConfig = ''
    gtk-toolbar-style = icons
    gtk-toolbar-icon-size = large-toolbar
    gtk-button-images = 0
    gtk-menu-images = 0
    gtk-enable-event-sounds = 1
    gtk-enable-input-feedback-sounds = 0
    gtk-xft-antialias = 1
    gtk-xft-hinting = 1
    gtk-xft-hintstyle = "hintslight"
    gtk-xft-rgba = "rgb"
    gtk-application-prefer-dark-theme = 1
  '';

  gtk3ExtraConfig = {
    gtk-enable-event-sounds = 1;
    gtk-enable-input-feedback-sounds = 0;
    gtk-xft-antialias = 1;
    gtk-xft-hinting = 1;
    gtk-xft-hintstyle = "hintslight";
    gtk-xft-rgba = "rgb";
    gtk-application-prefer-dark-theme = 1;
    gtk-overlay-scrolling = true;  # since 3.24.9
    gtk-recent-files-enabled = true;
    gtk-recent-files-max-age = 30;
    gtk-enable-primary-paste = true;  # since 3.4
    gtk-keynav-use-caret = true;  # since 3.20
    # (since 3.12+)
    gtk-dialogs-use-header = true;
    gtk-decoration-layout = "menu:minimize,maximize,close";
    # (since 3.14)
    gtk-titlebar-double-click = "toggle-maximize";
    gtk-titlebar-middle-click = "none";
    gtk-titlebar-right-click = "menu";
    
    # Deprecated
    gtk-toolbar-style = "GTK_TOOLBAR_ICONS";      # deprecated since 3.10
    gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";  # deprecated since 3.10
    gtk-button-images = 0;                        # deprecated since 3.10
    gtk-menu-images = 0;                          # deprecated since 3.10
  };

  gtk4ExtraConfig = {
    gtk-enable-event-sounds = 1;
    gtk-enable-input-feedback-sounds = 0;
    gtk-xft-antialias = 1;
    gtk-xft-hinting = 1;
    gtk-xft-hintstyle = "hintslight";
    gtk-xft-rgba = "rgb";
    
    # (since 4.20) ⚠️ <2025-09-14 Sun> gtk4-4.18.6_1
    # gtk-interface-color-scheme = "default";  # "dark", "light" 
    # gtk-interface-color-scheme = "dark";  
    # По умолчанию GTK_INTERFACE_COLOR_SCHEME_UNSUPPORTED 
    
    gtk-overlay-scrolling = true;
    gtk-recent-files-enabled = true;
    gtk-recent-files-max-age = 30;
    gtk-enable-primary-paste = true;
    gtk-keynav-use-caret = true;
    gtk-dialogs-use-header = true;
    gtk-decoration-layout = "menu:minimize,maximize,close";
    gtk-titlebar-double-click = "toggle-maximize";
    gtk-titlebar-middle-click = "none";
    gtk-titlebar-right-click = "menu";
    
    # GTK4-специфичные настройки
    gtk-primary-button-warps-slider = true;
    
    # Removed in GTK4 
    # gtk-toolbar-style - removed
    # gtk-toolbar-icon-size - removed  
    # gtk-button-images - removed
    # gtk-menu-images - removed
    gtk-application-prefer-dark-theme = 1; #- deprecated since 4.20, use gtk-interface-color-scheme
  };
in
{
  home.pointerCursor = {
    enable = true; 
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 24;             
    gtk.enable = true;     
    
    dotIcons.enable = false;         # ~/.icons 
    # hyprcursor.enable = true;      # Hyprland/Wayland
    # sway.enable = true;            # Sway
  };
  
  gtk = {
    enable = true;
    # cursorTheme = {
    #   name = "Bibata-Original-Ice-Right";
    #   size = 24;
    # };
    theme.name = "Greybird";
    # theme.name = "Yaru-dark";
    iconTheme.name = "Papirus-Light";
    font = {
      name = "Sans";
      size = 10;
    };
    gtk4.theme = config.gtk.theme;
    gtk2.extraConfig = gtk2ExtraConfig;
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    gtk3.extraConfig = gtk3ExtraConfig;
    gtk4.extraConfig = gtk4ExtraConfig;
  };
}
