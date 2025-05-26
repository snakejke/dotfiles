{
  programs.wget = {
    enable = true;
    config = ''
    hsts-file=~/.local/var/cache/wget-hsts
       '';
  };
}
