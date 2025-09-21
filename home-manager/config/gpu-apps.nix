{ config, lib, pkgs, nixgl, ... }:

{
  nixGL.packages = nixgl.packages;
  nixGL.defaultWrapper = "nvidia";
  nixGL.installScripts = [ "nvidia" ];

  programs.firefox = {
    enable = true;
    package = config.lib.nixGL.wrap pkgs.firefox;
    
    profiles.default = {
      id = 0;
      isDefault = true;
      
      settings = {
        "gfx.webrender.all" = true;
        "media.ffmpeg.vaapi.enabled" = true;
        "media.hardware-video-decoding.enabled" = true;
        "layers.acceleration.force-enabled" = true;
        "webgl.force-enabled" = true;
        "media.av1.enabled" = true;
      };
    };
  };
  # home.packages = with pkgs; [
    # (config.lib.nixGL.wrap discord)
    # (config.lib.nixGL.wrap google-chrome)
    # (config.lib.nixGL.wrap obs-studio)
    # (config.lib.nixGL.wrap blender)
  # ];
}
