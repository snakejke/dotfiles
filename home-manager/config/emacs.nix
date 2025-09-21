{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    (emacs-git.overrideAttrs (oldAttrs: {
      configureFlags = [
        "--with-native-compilation=aot"
        "--with-mailutils" 
        "--with-imagemagick"
        "--with-xinput2"
        "--without-compress-install"
        "--with-tree-sitter"
        "--with-rsvg"
        "--with-x-toolkit=lucid"
        "--without-toolkit-scroll-bars"
      ];
      
      NIX_CFLAGS_COMPILE = "-O2 -mtune=native -march=native -fomit-frame-pointer";
      NIX_ENFORCE_NO_NATIVE = "";
      
      buildInputs = oldAttrs.buildInputs ++ [
        mailutils
        imagemagick
        tree-sitter
        librsvg
      ];
      
      postInstall = (oldAttrs.postInstall or "") + ''
          if [ -f "$out/share/applications/emacsclient.desktop" ]; then
           substituteInPlace "$out/share/applications/emacsclient.desktop" \
      --replace "then exec emacsclient --alternate-editor=" "then exec $out/bin/emacsclient --create-frame --no-wait --alternate-editor="
          fi
          '';
    }))
  ];
}
