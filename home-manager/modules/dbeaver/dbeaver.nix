{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.programs.dbeaver;
  
  # Получаем имя пользователя и домашнюю директорию из конфигурации
  username = config.home.username;
  homeDir = config.home.homeDirectory;
  
  # Формируем путь для DBeaver на основе имени пользователя
  defaultUserHome = "-Duser.home=${homeDir}/.local/share/dbeaver";
  
  # Создаем кастомный пакет DBeaver
  customDBeaver = pkgs.dbeaver-bin.overrideAttrs (oldAttrs: {
    # Переопределяем prePatch для добавления настроек
    prePatch = ''
      ${oldAttrs.prePatch or ""}
      
      # Добавляем -vm опцию перед -vmargs, затем пользовательскую настройку после -vmargs
      awk '
      {
        if ($0 == "-vmargs") {
          print "-vm";
          print "${cfg.vmPath}";
          print $0;
          print "${cfg.userHome}";
        } else {
          print $0;
        }
      }' dbeaver.ini > dbeaver.ini.new
      mv dbeaver.ini.new dbeaver.ini
    '';
  });
in {
  options.programs.dbeaver = {
    enable = mkEnableOption "DBeaver database tool";
    
    vmPath = mkOption {
      type = types.str;
      default = "${homeDir}/.local/devjava/sdkman/candidates/java/current/bin";
      description = "Path to Java VM binary directory.";
    };
    
    userHome = mkOption {
      type = types.str;
      default = defaultUserHome;
      description = "Custom user.home directory for DBeaver.";
      example = "-Duser.home=/home/username/.local/share/dbeaver";
    };
    
    maxHeapSize = mkOption {
      type = types.str;
      default = "1024m";
      description = "Maximum heap size for Java (-Xmx).";
    };
  };
  
  config = mkIf cfg.enable {
    home.packages = [
      (customDBeaver.override {
        override_xmx = cfg.maxHeapSize;
      })
    ];
  };
}
