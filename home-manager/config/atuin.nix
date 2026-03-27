{
    programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    daemon.enable = false;

    
    settings = {
      style = "compact";
      search_mode = "daemon-fuzzy"; # Atuin 18.13
      inline_height = 23;
      show_help = false;
      logs.dir = "~/.cache/";
      daemon = {
        enabled = true;
        autostart = false;
      };
    };
  };
}
