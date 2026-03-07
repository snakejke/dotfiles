{
  description = "Build tree-sitter grammars with optimized flags -march=native -O3 -fPIC";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Import generated sources from nvfetcher
        sources = import ./_sources/generated.nix {
          inherit (pkgs)
            fetchgit
            fetchurl
            fetchFromGitHub
            dockerTools
            ;
        };

        # Custom grammar builder with optimized flags
        buildGrammar =
          {
            name,
            src,
            location ? null,
            generate ? false,
          }:
          pkgs.stdenv.mkDerivation {
            pname = "tree-sitter-${name}";
            version = "latest";

            inherit src;

            nativeBuildInputs =
              [
                pkgs.nodejs
              ]
              ++ pkgs.lib.optionals generate [ pkgs.tree-sitter ];

            CFLAGS = [
              "-Isrc"
              "-march=native"
              "-O3"
              "-fPIC"
            ];
            CXXFLAGS = [
              "-Isrc"
              "-march=native"
              "-O3"
              "-fPIC"
            ];

            configurePhase =
              pkgs.lib.optionalString (location != null) ''
                cd ${location}
              ''
              + pkgs.lib.optionalString generate ''
                tree-sitter generate
              '';

            buildPhase = ''
              runHook preBuild
              if [[ -e src/scanner.cc ]]; then
                $CXX -fPIC -march=native -O3 -c src/scanner.cc -o scanner.o $CXXFLAGS
              elif [[ -e src/scanner.c ]]; then
                $CC -fPIC -march=native -O3 -c src/scanner.c -o scanner.o $CFLAGS
              fi
              $CC -fPIC -march=native -O3 -c src/parser.c -o parser.o $CFLAGS
              rm -rf parser
              $CXX -fPIC -march=native -O3 -shared -o parser *.o
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              mv parser $out/
              if [[ -d queries ]]; then
                cp -r queries $out
              fi
              runHook postInstall
            '';
          };

        # Grammar configurations
        grammarConfigs = [
          { name = "bash"; location = null; generate = false; }
          { name = "c"; location = null; generate = false; }
          { name = "c-sharp"; location = null; generate = false; }
          { name = "c3"; location = null; generate = false; }
          { name = "clojure"; location = null; generate = false; }
          { name = "cmake"; location = null; generate = false; }
          { name = "cpp"; location = null; generate = false; }
          { name = "css"; location = null; generate = false; }
          { name = "dart"; location = null; generate = false; }
          { name = "dockerfile"; location = null; generate = false; }
          { name = "doxygen"; location = null; generate = false; }
          { name = "elisp"; location = null; generate = false; }
          { name = "elixir"; location = null; generate = false; }
          { name = "erlang"; location = null; generate = false; }
          { name = "glsl"; location = null; generate = false; }
          { name = "go"; location = null; generate = false; }
          { name = "gomod"; location = null; generate = false; }
          { name = "gpr"; location = null; generate = false; }
          { name = "haskell"; location = null; generate = false; }
          { name = "heex"; location = null; generate = false; }
          { name = "html"; location = null; generate = false; }
          { name = "janet-simple"; location = null; generate = false; }
          { name = "javascript"; location = null; generate = false; }
          { name = "java"; location = null; generate = false; }
          { name = "jsdoc"; location = null; generate = false; }
          { name = "json"; location = null; generate = false; }
          { name = "julia"; location = null; generate = false; }
          { name = "kotlin"; location = null; generate = false; }
          { name = "lua"; location = null; generate = false; }
          { name = "magik"; location = null; generate = false; }
          { name = "make"; location = null; generate = false; }
          { name = "markdown"; location = "tree-sitter-markdown"; generate = false; }
          { name = "markdown-inline"; location = "tree-sitter-markdown-inline"; generate = false; }
          { name = "nix"; location = null; generate = false; }
          { name = "org"; location = null; generate = false; }
          { name = "perl"; location = null; generate = false; }
          { name = "php"; location = "php"; generate = false; }
          { name = "proto"; location = null; generate = false; }
          { name = "python"; location = null; generate = false; }
          { name = "ruby"; location = null; generate = false; }
          { name = "rust"; location = null; generate = false; }
          { name = "scala"; location = null; generate = false; }
          { name = "scss"; location = null; generate = false; }
          { name = "sdml"; location = null; generate = false; }
          { name = "souffle"; location = null; generate = false; }
          { name = "sql"; location = null; generate = true; }
          { name = "surface"; location = null; generate = false; }
          { name = "toml"; location = null; generate = false; }
          { name = "tsx"; location = "tsx"; generate = false; }
          { name = "typescript"; location = "typescript"; generate = false; }
          { name = "typst"; location = null; generate = false; }
          { name = "vala"; location = null; generate = false; }
          { name = "verilog"; location = null; generate = false; }
          { name = "vhdl"; location = null; generate = false; }
          { name = "wgsl"; location = null; generate = false; }
          { name = "yaml"; location = null; generate = false; }
          { name = "zig"; location = null; generate = false; }
        ];

        # Build all grammars
        builtGrammars = pkgs.lib.listToAttrs (
          map
            (g: {
              name = g.name;
              value = buildGrammar {
                inherit (g) name location generate;
                src = sources.${g.name}.src;
              };
            })
            grammarConfigs
        );

        # Package to copy all .so files
        allGrammarsPackage = pkgs.runCommand "tree-sitter-all-grammars" { } ''
          mkdir -p $out
          ${pkgs.lib.concatStrings (
            pkgs.lib.mapAttrsToList (
              name: drv: ''
                cp ${drv}/parser $out/${name}.so
              ''
            ) builtGrammars
          )}
        '';

      in
      {
        packages = {
          default = allGrammarsPackage;
          all-grammars = allGrammarsPackage;
        } // builtGrammars;

        apps = {
          # Копирование грамматик с правильными именами (libtree-sitter-*.so)
          copy-grammars = flake-utils.lib.mkApp {
            drv = pkgs.writeShellScriptBin "copy-grammars" ''
              set -e
              
              # Целевая директория (по умолчанию текущая)
              TARGET_DIR="''${1:-$(pwd)}"
              
              echo "Копирование грамматик в $TARGET_DIR..."
              mkdir -p "$TARGET_DIR"
              
              ${pkgs.lib.concatStrings (
                pkgs.lib.mapAttrsToList (
                  name: drv: ''
                    echo "  ${name}..."
                    cp "${drv}/parser" "$TARGET_DIR/libtree-sitter-${name}.so"
                  ''
                ) builtGrammars
              )}
              
              echo "Готово! Скопировано ${builtins.toString (builtins.length (builtins.attrNames builtGrammars))} грамматик."
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.tree-sitter ];
        };
      }
    );
}
