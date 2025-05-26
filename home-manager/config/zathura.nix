{
  programs.zathura = {
    enable = true;
    
    options = {
      selection-clipboard = "clipboard";
      adjust-open = "width";
      statusbar-h-padding = 0;
      statusbar-v-padding = 0;
      page-padding = 1;
      window-title-basename = true;
      font = "IBM Plex Serif 10";
      
      # Цветовая схема
      default-bg = "#000000";
      default-fg = "#F2F3F4";
      render-loading = true;
      render-loading-bg = "#F2F3F4";
      render-loading-fg = "#000000";
      recolor-lightcolor = "#F2F3F4";
      recolor-darkcolor = "#000000";
      recolor = true;
      recolor-keephue = true;
    };
    
    mappings = {
      "e" = "scroll half-up";
      "d" = "scroll half-down";
      "D" = "toggle_page_mode";
      "r" = "reload";
      "R" = "rotate";
      "K" = "zoom in";
      "J" = "zoom out";
      "i" = "recolor";
      "p" = "print";
      "g" = "goto top";
    };
  };
}
