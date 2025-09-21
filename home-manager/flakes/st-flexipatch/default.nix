{ pkgs, st-flexipatch-pkg, ... }:

{
  home.packages = [ st-flexipatch-pkg ];
  
  # xresources.properties = {
  #   "st.font" = "JetBrainsMono Nerd Font:pixelsize=14";
  #   "st.alpha" = 0.9;
  # };
}
