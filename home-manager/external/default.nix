{ pkgs, lib }:
let
  sources = pkgs.callPackage ./_sources/generated.nix {};
  
  # Автоматически находим все .nix файлы в packages/
  packageFiles = builtins.attrNames (lib.filterAttrs 
    (name: type: type == "regular" && lib.hasSuffix ".nix" name)
    (builtins.readDir ./packages)
  );
  
  # Создаем список пакетов (не атрибут сет!)
  packages = map (file: 
    pkgs.callPackage (./packages + "/${file}") {
      inherit sources;
    }
  ) packageFiles;
  
in packages
