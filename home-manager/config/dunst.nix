{ config, pkgs, ... }:
{
  services.dunst = {
    enable = true;
    # iconTheme = {
    #   # package = pkgs.adwaita-icon-theme;
    #   # name = "Adwaita";
    #   name = "Tela";
    #   package = pkgs.tela-icon-theme;
    #   size = "22x22";
    # };
    settings = {
      global = {
        monitor = 0;
        icon_theme = "Papirus";
        follow = "none";
        width = 400;
        height = "(0, 400)";
        origin = "top-right";
        offset = "(10, 50)";
        scale = 0;
        notification_limit = 20;
        progress_bar = true;
        progress_bar_height = 10;
        progress_bar_frame_width = 1;
        progress_bar_min_width = 150;
        progress_bar_max_width = 300;
        progress_bar_corner_radius = 0;
        icon_corner_radius = 0;
        indicate_hidden = true;
        transparency = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        text_icon_padding = 0;
        frame_width = 3;
        frame_color = "#aaaaaa";
        gap_size = 0;
        separator_color = "frame";
        sort = true;
        font = "Monospace 11";
        line_height = 0;
        markup = "full";
        format = "<b>%s</b>: %b";
        alignment = "left";
        vertical_alignment = "center";
        show_age_threshold = 60;
        ellipsize = "middle";
        ignore_newline = false;
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = true;
        enable_recursive_icon_lookup = true;
        icon_position = "left";
        min_icon_size = 32;
        max_icon_size = 128;
        sticky_history = true;
        history_length = 20;
        dmenu = "/usr/bin/dmenu -p dunst:";
        browser = "/usr/bin/xdg-open";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        corner_radius = 0;
        ignore_dbusclose = false;
        force_xwayland = false;
        force_xinerama = false;
        mouse_left_click = "close_current";
        mouse_middle_click = "do_action, close_current";
        mouse_right_click = "close_all";
      };
      experimental = {
        per_monitor_dpi = false;
      };
      urgency_low = {
        background = "#222222";
        foreground = "#888888";
        timeout = 10;
      };
      urgency_normal = {
        background = "#ffffff";
        foreground = "#595959";
        timeout = 10;
        override_pause_level = 30;
      };
      urgency_critical = {
        background = "#900000";
        foreground = "#ffffff";
        frame_color = "#ff0000";
        timeout = 0;
        override_pause_level = 60;
      };
    };
  };
}
