{ config, ... }:

{
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

}
