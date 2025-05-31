{ pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    shell = "/usr/bin/zsh";
    terminal = "tmux-256color";
    prefix = "C-space";
    baseIndex = 1;
    escapeTime = 10;
    focusEvents = true;
    mouse = true;
    
    plugins = with pkgs.tmuxPlugins; [
      sensible
      {
        plugin = mode-indicator;
        extraConfig = ''
          set -g @mode_indicator_prefix_prompt " WAIT "
          set -g @mode_indicator_prefix_mode_style fg=#61afef,bold
          set -g @mode_indicator_copy_prompt " COPY "
          set -g @mode_indicator_copy_mode_style fg=#98c379,bold
          set -g @mode_indicator_sync_prompt " SYNC "
          set -g @mode_indicator_sync_mode_style fg=#e06c75,bold
          set -g @mode_indicator_empty_prompt " TMUX "
          set -g @mode_indicator_empty_mode_style fg=#2e323b,bg=#e06c75,bold
        '';
      }
      {
        plugin = cpu;
        extraConfig = ''
          set -g @cpu_percentage_format "%3.0f%%"
        '';
      }
      open
      {
        plugin = yank;
        extraConfig = ''
          set -g @yank_with_mouse on
        '';
      }
      {
        plugin = resurrect;
        extraConfig = ''
          set -g @resurrect-save 'S'
          set -g @resurrect-restore 'R'
          set -g @resurrect-strategy-nvim 'session'
        '';
      }
      {
        plugin = online-status;
        extraConfig = ''
          set -g @route_to_ping "google.com"
          set -g @online_icon "#[fg=#e06c75,bold] 󰌘"
          set -g @offline_icon "#[fg=#e06c75,bold] 󰌙"
        '';
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'off'
        '';
      }
    ];

    extraConfig = ''
      # Terminal and color settings
      set -ag terminal-overrides ",*256col*:RGB"
      
      # Custom key bindings
      bind r source-file ~/.config/tmux/tmux.conf \; display "tmux.conf reloaded!"
      unbind '"'
      unbind %
      bind - split-window -h
      bind | split-window -v
      
      # Sticky time after repeated commands
      set -sg repeat-time 500
      
      # Status bar configuration
      set -g visual-activity off
      set -g visual-bell off
      set -g visual-silence off
      setw -g monitor-activity off
      set -g bell-action none
      
      # Clock and mode styling
      setw -g clock-mode-colour colour5
      setw -g mode-style 'fg=colour1 bg=colour18 bold'
      
      # Status bar theme
      set -g status-position bottom
      set -g status-justify centre
      set -g status-style 'bg=colour18 fg=colour137 dim'
      set -g status-left '#{tmux_mode_indicator} #{online_status} #[fg=#e06c75]%R '
      set -g status-right '#[fg=colour233,bg=colour19] %d/%m #[fg=colour233,bg=colour8] %H:%M:%S '
      set -g status-right-length 90
      set -g status-left-length 90
      
      # Window status styling
      setw -g window-status-current-style 'fg=colour1 bg=colour19 bold'
      setw -g window-status-current-format ' #I#[fg=colour249]:#[fg=colour255]#W#[fg=colour249]#F '
      setw -g window-status-style 'fg=colour9 bg=colour18'
      setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '
      setw -g window-status-bell-style 'fg=colour255 bg=colour1 bold'
      
      # Message styling
      set -g message-style 'fg=colour232 bg=colour16 bold'
    '';
  };
}
