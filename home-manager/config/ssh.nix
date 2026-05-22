{
  programs.ssh = {
    enable = true;
    package = null;
    
    enableDefaultConfig = false;
    
    settings = {
      "github-gmail" = {
        HostName = "github.com";
        User = "git";
        IdentityFile = "~/.ssh/id_ed25519_gmail";
        IdentitiesOnly = true;
      };
      
      "github.com" = {
        HostName = "github.com";
        User = "git";
        IdentityFile = "~/.ssh/id_ed25519";
      };

      "android" = {
        HostName = "192.168.0.14";
        User = "u0_a680";
        Port = 8022;
      };
      
      "*" = {
        IdentityFile = "~/.ssh/id_ed25519_gmail";
        ForwardAgent = false;
        AddKeysToAgent = "yes";
        Compression = false;
        ServerAliveInterval = 0;
        ServerAliveCountMax = 3;
        HashKnownHosts = false;
        UserKnownHostsFile = "~/.ssh/known_hosts";
        ControlMaster = "no";
        ControlPath = "~/.ssh/master-%r@%n:%p";
        ControlPersist = "no";
      };
    };
  };
}
