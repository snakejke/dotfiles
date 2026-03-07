{
  description = "st-flexipatch build flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        st-flexipatch = pkgs.stdenv.mkDerivation {
          pname = "st-flexipatch";
          version = "0.9.3";

          src = pkgs.fetchFromGitHub {
            owner = "bakkeby";
            repo = "st-flexipatch";
            rev = "bdb21ddb8bdffa372b2870407a2869d5c5a822f3";
            sha256 = "sha256-U5jE+vQJJODmpUA5eWZ6BNPG/uLSi70WLoFUAWt0kuA=";
          };

          outputs = [ "out" "terminfo" ];
          strictDeps = true;

          nativeBuildInputs = with pkgs; [
            pkg-config ncurses fontconfig freetype harfbuzz
          ];

          buildInputs = with pkgs; [
            libX11 libXft libXcursor libXinerama
            libXrender libXext libXrandr
            harfbuzz gd imlib2 libsixel
          ];

          postPatch = ''
            cp ${./patches.h} patches.h
            cp ${./config.h} config.def.h

            sed -i 's|PKG_CONFIG = pkg-config|PKG_CONFIG = ${pkgs.stdenv.cc.targetPrefix}pkg-config|' config.mk

            # patches.h
            
            # LIGATURES_PATCH
            if grep -q "#define LIGATURES_PATCH 1" patches.h; then
              sed -i '/# Uncomment the lines below for the ligatures patch/,/^$/s/^#LIGATURES/LIGATURES/' config.mk
            fi

            # ALPHA_PATCH  
            if grep -q "#define ALPHA_PATCH 1" patches.h; then
              sed -i 's/^#XRENDER = /XRENDER = /' config.mk
            fi

            # THEMED_CURSOR_PATCH
            if grep -q "#define THEMED_CURSOR_PATCH 1" patches.h; then
              sed -i 's/^#XCURSOR = /XCURSOR = /' config.mk
            fi

            # SIXEL_PATCH
            if grep -q "#define SIXEL_PATCH 1" patches.h; then
              sed -i '/# Uncomment this for the SIXEL patch/,/^$/s/^#SIXEL/SIXEL/' config.mk
            fi

            # NETWMICON_PATCH
            if grep -q "#define NETWMICON_PATCH 1" patches.h; then
              sed -i 's/^#NETWMICON_LIBS = /NETWMICON_LIBS = /' config.mk
            fi

          '';

          makeFlags = [
            "PKG_CONFIG=${pkgs.stdenv.cc.targetPrefix}pkg-config"
          ];

          preInstall = ''
            export TERMINFO=$terminfo/share/terminfo
            mkdir -p $TERMINFO $out/nix-support
            echo "$terminfo" >> $out/nix-support/propagated-user-env-packages
          '';

          installFlags = [ "PREFIX=$(out)" ];

          meta = with pkgs.lib; {
            description = "Simple Terminal for X with flexipatch";
            homepage = "https://github.com/bakkeby/st-flexipatch";
            license = licenses.mit;
            platforms = platforms.unix;
            mainProgram = "st";
          };
        };
      in
      {
        packages.default = st-flexipatch;
      }
    );
}
