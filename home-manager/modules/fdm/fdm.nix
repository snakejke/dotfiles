{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.fdm;
  
  # Helper function to find index of element in list (replacement for lib.findFirstIndex)
  findIndex = pred: list: 
    let
      len = lib.length list;
      helper = idx:
        if idx >= len then -1
        else if pred (lib.elemAt list idx) then idx
        else helper (idx + 1);
    in helper 0;
  
  # Helper functions for config generation
  optionalConfig = name: value:
    lib.optionalString (value != null) "${name} ${value}\n";
  
  numericConfig = name: value: 
    lib.optionalString (value != null) "${name} ${toString value}\n";
  
  # Generate variables section - FIXED: using custom findIndex function
  variablesSection = 
    let
      # Define preferred order for variables
      preferredOrder = ["mail_base" "cache_file"];
      
      # Sort variables by preferred order, then alphabetically
      sortedVars = lib.sort (a: b: 
        let
          aIdx = findIndex (x: x == a.name) preferredOrder;
          bIdx = findIndex (x: x == b.name) preferredOrder;
          aPos = if aIdx == -1 then 999 else aIdx;
          bPos = if bIdx == -1 then 999 else bIdx;
        in
        if aPos != bPos then aPos < bPos else a.name < b.name
      ) (lib.mapAttrsToList (name: value: { inherit name value; }) cfg.variables);
    in
    lib.concatStringsSep "\n" (
      map (var: "$" + var.name + " = \"" + var.value + "\"") sortedVars
    );
  
  # Generate actions section - FIXED: using custom findIndex function
  actionsSection = 
    let
      # Define preferred order for actions
      preferredOrder = ["inbox" "drafts" "sent" "spam" "trash"];
      
      # Sort actions by preferred order, then alphabetically
      sortedActions = lib.sort (a: b:
        let
          aIdx = findIndex (x: x == a.name) preferredOrder;
          bIdx = findIndex (x: x == b.name) preferredOrder;
          aPos = if aIdx == -1 then 999 else aIdx;
          bPos = if bIdx == -1 then 999 else bIdx;
        in
        if aPos != bPos then aPos < bPos else a.name < b.name
      ) (lib.mapAttrsToList (name: config: { inherit name config; }) cfg.actions);
    in
    lib.concatStringsSep "\n" (
      map (action: ''action "${action.name}" ${action.config}'') sortedActions
    );
  
  # Generate accounts section - FIXED: improved folders formatting
  accountsSection = lib.concatStringsSep "\n" (
    map (account: 
      let
        foldersStr = if account.folders != [] then
          let
            folderList = lib.concatStringsSep "\n            " 
              (map (f: "\"${f}\"") account.folders);
          in
          "    folders {\n            ${folderList}\n            }"
        else "";
      in
      ''
        account "${account.name}" ${account.type} server "${account.server}"
            user "${account.user}" pass ${account.password}
        ${lib.optionalString (foldersStr != "") foldersStr}
      ''
    ) cfg.accounts
  );
  
  # Generate match rules section  
  matchRulesSection = lib.concatStringsSep "\n" cfg.matchRules;
  
  # Generate final config - FIXED: proper section ordering
  fdmConfig = lib.concatStringsSep "\n" (lib.filter (s: s != "") [
    # Variables first
    (lib.optionalString (cfg.variables != {}) variablesSection)
    
    # Parallel accounts setting
    (numericConfig "set parallel-accounts" cfg.parallelAccounts)
    
    # Cache configuration (appears after variables but before actions)
    (lib.optionalString (cfg.cacheFile != null && cfg.cacheExpire != null) 
      "cache ${cfg.cacheFile} expire ${cfg.cacheExpire}")
    
    # Actions
    (lib.optionalString (cfg.actions != {}) actionsSection)
    
    # Accounts  
    (lib.optionalString (cfg.accounts != []) accountsSection)
    
    # Match rules
    (lib.optionalString (cfg.matchRules != []) matchRulesSection)
    
    # Default match rule
    (lib.optionalString (cfg.defaultAction != null) "match all action \"${cfg.defaultAction}\"")
    
    # Extra config
    (lib.optionalString (cfg.extraConfig != "") cfg.extraConfig)
  ]);
in
with lib;
{
  options.programs.fdm = {
    enable = mkEnableOption "fdm mail fetcher";
    
    package = mkPackageOption pkgs "fdm" { };
    
    variables = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = ''
        FDM variables to define at the top of config.
        Example: { mail_base = "%h/Mail"; }
      '';
      example = {
        mail_base = "%h/Mail";
        cache_file = "%h/Mail/cache.db";
      };
    };
    
    parallelAccounts = mkOption {
      type = types.nullOr types.ints.unsigned;
      default = null;
      description = "Number of accounts to process in parallel";
      example = 2;
    };
    
    cacheFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Path to cache file (can use variables like $cache_file)";
      example = "$cache_file";
    };
    
    cacheExpire = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Cache expiration time";
      example = "1 month";
    };
    
    actions = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = ''
        FDM actions definition.
        Key is action name, value is the action configuration.
      '';
      example = {
        inbox = ''maildir "%h/Mail/INBOX"'';
        spam = ''maildir "%h/Mail/spam"'';
      };
    };
    
    accounts = mkOption {
      type = types.listOf (types.submodule {
        options = {
          name = mkOption {
            type = types.str;
            description = "Account name";
          };
          
          type = mkOption {
            type = types.enum ["imaps" "imap" "pop3" "pop3s"];
            default = "imaps";
            description = "Account type";
          };
          
          server = mkOption {
            type = types.str;
            description = "Server hostname";
          };
          
          user = mkOption {
            type = types.str;
            description = "Username";
          };
          
          password = mkOption {
            type = types.str;
            description = ''
              Password or password command.
              Example: $(gpg --quiet --decrypt ~/.password-store/email.gpg)
            '';
          };
          
          folders = mkOption {
            type = types.listOf types.str;
            default = [];
            description = "List of folders to fetch from";
          };
        };
      });
      default = [];
      description = "List of mail accounts to configure";
    };
    
    matchRules = mkOption {
      type = types.listOf types.lines;
      default = [];
      description = ''
        List of match rules as strings.
        Each element should be a complete match block.
      '';
      example = [
        ''
          match string "%[account]" to "Gmail" {
              match string "%[folder]" to "INBOX" action "inbox"
              match string "%[folder]" to "[Gmail]/Spam" action "spam"
          }
        ''
      ];
    };
    
    defaultAction = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Default action for unmatched mail";
      example = "inbox";
    };
    
    extraConfig = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Additional FDM configuration to append.
        This allows adding any custom settings not covered by specific options.
      '';
    };
  };
  
  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    
    xdg.configFile."fdm/.fdm.conf".text = fdmConfig;
  };
}
