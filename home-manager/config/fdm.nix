{ config, pkgs, ... }:

{
  programs.fdm = {
    enable = true;
    
    variables = {
      mail_base = "%h/Mail";
      cache_file = "%h/Mail/cache.db";
    };
    
    parallelAccounts = 2;
    cacheFile = "$cache_file"; 
    cacheExpire = "1 month";
    
    actions = {
      inbox = ''maildir "%h/Mail/INBOX"'';
      drafts = ''maildir "%h/Mail/drafts"'';
      sent = ''maildir "%h/Mail/sent"'';
      spam = ''maildir "%h/Mail/spam"'';
      trash = ''maildir "%h/Mail/trash"'';
    };
    
    accounts = [
      {
        name = "Gmail";
        type = "imaps";
        server = "imap.gmail.com";
        user = "snake05865@gmail.com";
        password = "$(gpg --quiet --decrypt ~/.password-store/Email/apps/fdmgmail.gpg)";
        folders = [
          "INBOX"
          "[Gmail]/All Mail"
          "[Gmail]/Drafts"
          "[Gmail]/Important"
          "[Gmail]/Sent Mail"
          "[Gmail]/Spam"
          "[Gmail]/Starred"
          "[Gmail]/Trash"
        ];
      }
      {
        name = "Mailru";
        type = "imaps";
        server = "imap.mail.ru";
        user = "snake05865@mail.ru";
        password = "$(gpg --quiet --decrypt ~/.password-store/Email/apps/mailru.gpg)";
        folders = [
          "INBOX"
          "&BCEEPwQwBDw-"             # spam
          "&BB4EQgQ,BEAEMAQyBDsENQQ9BD0ESwQ1-"  # sent 
          "&BCcENQRABD0EPgQyBDgEOgQ4-"          # drafts
          "&BBoEPgRABDcEOAQ9BDA-"              # trash
          "INBOX/News"    
          "INBOX/Receipts"
        ];
      }
    ];
    
    matchRules = [
      ''
        match string "%[account]" to "Gmail" {
            # First check for duplicates using cache
            # match not string "%[message_id]" to "" {
            #     match in-cache $cache_file key "%[message_id]" action "trash"
            #     match all action add-to-cache $cache_file key "%[message_id]" continue
            # }
            
            match string "%[folder]" to "INBOX" action "inbox"
            # match string "%[folder]" to "[Gmail]/All Mail" action "inbox"
            match string "%[folder]" to "[Gmail]/Drafts" action "drafts"
            match string "%[folder]" to "[Gmail]/Important" action "inbox"
            match string "%[folder]" to "[Gmail]/Sent Mail" action "sent"
            match string "%[folder]" to "[Gmail]/Spam" action "spam"
            match string "%[folder]" to "[Gmail]/Starred" action "inbox"
            match string "%[folder]" to "[Gmail]/Trash" action "trash"
        }
      ''
      ''
        match string "%[account]" to "Mailru" {
              match string "%[folder]" to "INBOX" action "inbox"
              match string "%[:folder]" to "&BCEEPwQwBDw-" action "spam"
              match string "%[:folder]" to "&BB4EQgQ,BEAEMAQyBDsENQQ9BD0ESwQ1-" action "sent" 
              match string "%[:folder]" to "&BCcENQRABD0EPgQyBDgEOgQ4-" action "drafts"
              match string "%[:folder]" to "&BBoEPgRABDcEOAQ9BDA-" action "trash"
              match string "%[folder]" to "INBOX/News" action "inbox"
              match string "%[folder]" to "INBOX/Receipts" action "inbox"
              #match string "%[:folder]" to "&BCcENQRABD0EPgQyBDgEOgQ4-" actions { "add-folder-header" "process-mail" }
        }
      ''
    ];
    
    defaultAction = "inbox";
    
    extraConfig = ''
      # strip-characters https://github.com/nicm/fdm/blob/0918b78a82a789d63cebe44b7662f0a8dc603000/CHANGES#L237C2-L237C22
      
      # action "add-folder-header" {
      #     add-header "X-FDM-Folder" value "Drafts"
      # }
      # action "process-mail" {
      #     pipe "/usr/bin/maildrop -V 3 -d"
      # }
    '';
  };
}
