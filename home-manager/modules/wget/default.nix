{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.wget;
  
  # Helper function to conditionally add config lines
  optionalConfig = name: value:
    lib.optionalString (value != null) "${name} = ${value}\n";
  
  # Helper function for numeric values (always present)
  numericConfig = name: value: "${name} = ${toString value}\n";
  
  wgetConf = lib.concatStrings [
    (optionalConfig "check_certificate" cfg.checkCertificate)
    (optionalConfig "timestamping" cfg.timestamping)
    (optionalConfig "no_parent" cfg.noParent)
    (numericConfig "timeout" cfg.timeout)
    (numericConfig "tries" cfg.tries)
    (optionalConfig "retry_connrefused" cfg.retryConnrefused)
    (optionalConfig "trust_server_names" cfg.trustServerNames)
    (optionalConfig "follow_ftp" cfg.followFtp)
    (optionalConfig "adjust_extension" cfg.adjustExtension)
    (optionalConfig "local_encoding" cfg.localEncoding)
    (optionalConfig "robots" cfg.robots)
    (optionalConfig "server_response" cfg.serverResponse)
    # Add custom config at the end
    (lib.optionalString (cfg.config != "") "${cfg.config}\n")
  ];
in
with lib;
{
  options.programs.wget = {
    enable = mkEnableOption "wget downloader";
    package = mkPackageOption pkgs "wget" { };
    
    checkCertificate = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Set certificate checking behavior. Use "off" to disable certificate verification.
        WARNING: Disabling certificate verification reduces security.
      '';
    };
    
    timestamping = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Use the server-provided last modification date, if available.
        Set to "on" to enable timestamping.
      '';
    };
    
    noParent = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Do not go up in the directory structure when downloading recursively.
        Set to "on" to enable this restriction.
      '';
    };
    
    timeout = mkOption {
      type = types.ints.unsigned;
      default = 15;
      description = ''
        Network timeout in seconds. This applies to DNS, connect and read timeouts.
      '';
    };
    
    tries = mkOption {
      type = types.ints.unsigned;
      default = 20;
      description = ''
        Number of retry attempts when a download fails.
      '';
    };
    
    retryConnrefused = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Retry even when the connection was refused.
        Set to "on" to enable retrying on connection refusal.
      '';
    };
    
    trustServerNames = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Use the last component of a redirection URL for the local file name.
        Set to "on" to enable server name trusting.
      '';
    };
    
    followFtp = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Follow FTP links from HTML documents by default.
        Set to "on" to enable FTP link following.
      '';
    };
    
    adjustExtension = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Add appropriate extensions to downloaded files based on content type.
        Set to "on" to enable extension adjustment.
      '';
    };
    
    localEncoding = mkOption {
      type = types.nullOr types.str;
      default = "UTF-8";
      description = ''
        Character encoding for local file system operations.
      '';
    };
    
    robots = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Respect robots.txt and robot meta tags.
        Set to "off" to ignore robot restrictions.
      '';
    };
    
    serverResponse = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Print HTTP and FTP server responses.
        Set to "on" to enable response printing.
      '';
    };
    
    config = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Additional configuration to append to the wget configuration file.
        This allows adding custom settings not covered by the specific options.
      '';
    };
  };
  
  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."wget/wgetrc".text = wgetConf;
  };
}
