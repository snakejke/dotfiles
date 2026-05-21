{
  programs.ssh = {
    enable = true;
    package = null;
    
    enableDefaultConfig = false;
    
    matchBlocks = {
      "github-gmail" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519_gmail";
        identitiesOnly = true;
      };
      
      "github.com" = {
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_ed25519";
      };

      "android" = {
        hostname = "192.168.0.14";
        user = "u0_a680";
        port = 8022;
      };
      
      "*" = {
        identityFile = "~/.ssh/id_ed25519_gmail";
        forwardAgent = false;
        addKeysToAgent = "yes";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";
        controlMaster = "no";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "no";
      };
    };
  };
}
