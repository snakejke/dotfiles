{ xdg, ... }:
{
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/epub+zip" = [
        "org.pwmt.zathura-pdf-mupdf.desktop"
        "com.github.johnfactotum.Foliate.desktop"
      ];
      "application/json" = [ "emacsclient.desktop" ];
      "application/octet-stream" = [
        "emacsclient.desktop"
        "nvim.desktop"
      ];
      "application/pdf" = [
        "org.pwmt.zathura-pdf-mupdf.desktop"
        "zotero.desktop"
      ];
      "application/postscript" = [ "org.pwmt.zathura-ps.desktop" ];
      "application/sql" = [
        "emacsclient.desktop"
        "nvim.desktop"
      ];
      "application/subrip" = [ "emacsclient.desktop" ];
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = [
        "libreoffice-writer.desktop"
        "abiword.desktop"
        "emacsclient.desktop"
      ];
      "application/x-7z-compressed" = [ "emacsclient.desktop" ];
      "application/x-bat" = [ "emacsclient.desktop" ];
      "application/x-desktop" = [
        "emacsclient.desktop"
        "emacs.desktop"
      ];
      "application/x-executable" = [ "emacs.desktop" ];
      "application/x-fictionbook+xml" = [
        "emacsclient.desktop"
        "koodo-reader.desktop"
        "com.github.johnfactotum.Foliate.desktop"
        "org.pwmt.zathura-pdf-mupdf.desktop"
      ];
      "application/x-perl" = [ "emacsclient.desktop" ];
      "application/x-ruby" = [ "emacsclient.desktop" ];
      "application/x-shellscript" = [ "emacsclient.desktop" ];
      "application/x-theme" = [
        "emacsclient.desktop"
        "emacs.desktop"
      ];
      "application/x-zerosize" = [ "emacsclient.desktop" ];
      "application/xml" = [ "emacsclient.desktop" ];
      "audio/x-mpegurl" = [ "emacsclient.desktop" ];
      "audio/x-opus+ogg" = [ "mpv.desktop" ];
      "image/avif" = [ "nsxiv.desktop" ];
      "image/gif" = [
        "nsxiv.desktop"
        "gimp.desktop"
        "feh.desktop"
      ];
      "image/heif" = [ "nsxiv.desktop" ];
      "image/jpeg" = [
        "nsxiv.desktop"
        "google-chrome.desktop"
        "gimp.desktop"
      ];
      "image/png" = [
        "nsxiv.desktop"
        "google-chrome.desktop"
        "nsxiv-2.desktop"
        "gimp.desktop"
        "feh.desktop"
      ];
      "image/svg+xml" = [
        "nsxiv.desktop"
        "org.inkscape.Inkscape.desktop"
        "nvim.desktop"
        "emacsclient.desktop"
        "gimp.desktop"
      ];
      "image/vnd.djvu+multipage" = [
        "org.pwmt.zathura-pdf-mupdf.desktop"
        "com.github.johnfactotum.Foliate.desktop"
      ];
      "image/webp" = [ "nsxiv.desktop" ];
      "inode/directory" = [ "pcmanfm.desktop" ];
      "inode/socket" = [ "emacsclient.desktop" ];
      "inode/x-empty" = [ "emacsclient.desktop" ];
      "text/html" = [
        "google-chrome.desktop"
        "emacsclient.desktop"
      ];
      "text/javascript" = [ "emacsclient.desktop" ];
      "text/markdown" = [
        "emacsclient.desktop"
        "nvim.desktop"
      ];
      "text/org" = [ "emacsclient.desktop" ];
      "text/plain" = [
        "emacsclient.desktop"
        "emacs.desktop"
        "nvim.desktop"
      ];
      "text/vtt" = [ "emacsclient.desktop" ];
      "text/x-chdr" = [ "emacsclient.desktop" ];
      "text/x-clojure" = [ "emacsclient.desktop" ];
      "text/x-java" = [
        "emacsclient.desktop"
        "code-oss.desktop"
        "geany.desktop"
        "nvim.desktop"
      ];
      "text/x-lisp" = [ "emacsclient.desktop" ];
      "text/x-lua" = [ "emacsclient.desktop" ];
      "text/x-python" = [ "emacsclient.desktop" ];
      "text/x-python3" = [ "emacsclient.desktop" ];
      "text/x-script.python" = [ "emacsclient.desktop" ];
      "text/x-shellscript" = [ "emacsclient.desktop" ];
      "video/mp4" = [
        "mpv.desktop"
        "vlc.desktop"
      ];
      "video/x-matroska" = [
        "mpv.desktop"
        "vlc.desktop"
        "org.xfce.Parole.desktop"
      ];
      "x-scheme-handler/about" = [ "google-chrome.desktop" ];
      "x-scheme-handler/eclipse+command" = [ "_usr_lib_dbeaver_.desktop" ];
      "x-scheme-handler/eclipse+mpc" = [ "_usr_lib_eclipse_.desktop" ];
      "x-scheme-handler/http" = [ "google-chrome.desktop" ];
      "x-scheme-handler/https" = [ "google-chrome.desktop" ];
      "x-scheme-handler/jetbrains" = [ "jetbrainsd.desktop" ];
      "x-scheme-handler/koodo-reader" = [ "koodo-reader.desktop" ];
      "x-scheme-handler/mailto" = [ "notmuch-emacs.desktop" ];
      "x-scheme-handler/tg" = [ "telegram-web.desktop" ];
      "x-scheme-handler/unknown" = [ "google-chrome.desktop" ];
    };
  };
}
