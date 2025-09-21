{ pkgs, lib }:
let
  sources = pkgs.callPackage ./_sources/generated.nix {};
  
  packageFiles = builtins.attrNames (lib.filterAttrs 
    (name: type: type == "regular" && lib.hasSuffix ".nix" name)
    (builtins.readDir ./packages)
  );
  
  packages = map (file: 
    pkgs.callPackage (./packages + "/${file}") {
      inherit sources;
    }
  ) packageFiles;
  
in packages
