{ config, ... }:

{
  xdg.desktopEntries = {

    discord = {
      name = "Discord";
      exec = "sh -c \"exec /usr/lib/discord/discord >/dev/null 2>&1\"";
      icon = "discord";
      comment = "All-in-one voice and text chat";
      genericName = "Internet Messenger";
      categories = [
        "Network"
        "InstantMessaging"
      ];
      settings.StartupWMClass = "discord";
    };

    eclipse-mat = {
      name = "Eclipse MAT";
      icon = "${config.home.homeDirectory}/.local/devjava/EclipseMAT/mat-logo.svg";
      comment = "Eclipse Memory Analyzer Tool (MAT), a toolkit for analyzing Java heap dumps.";
      exec = "${config.home.homeDirectory}/.local/devjava/EclipseMAT/MemoryAnalyzer";
      terminal = false;
      type = "Application";
      categories = [ "Development" ];
    };

    telegram-web = {
      name = "Telegram Web";
      comment = "Open Telegram links in the browser";
      exec = "telegram-handler.sh %u";
      terminal = false;
      type = "Application";
      mimeType = [ "x-scheme-handler/tg" ];
    };

    visualvm = {
      name = "VisualVM";
      icon = "visualvm";
      exec = "/usr/bin/zsh -c \"source ${config.home.homeDirectory}/.config/zsh/.zshenv && ${config.home.homeDirectory}/.local/bin/run-visualvm.sh\"";
      terminal = false;
      type = "Application";
      categories = [ "Development" ];
    };

    notmuch-emacs = {
      name = "Notmuch";
      genericName = "Email Client(Fixed)";
      comment = "Emacs based email client";
      exec = "notmuch-emacs-mua-my --hello %u";
      icon = "emblem-mail";
      type = "Application";
      terminal = false;
      startupNotify = false;

      categories = [
        "Network"
        "Email"
      ];
      mimeType = [ "x-scheme-handler/mailto" ];

      settings = {
        "Keywords" = "Mail;E-mail;Email;";
        "X-Desktop-File-Install-Version" = "0.27";
        "Path" = "";
      };
    };
  };
}
