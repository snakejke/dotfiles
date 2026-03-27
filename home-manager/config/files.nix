{ config, pkgs, ... }:
{
  home.file.".config/python/pythonrc".text = ''
    import readline
    readline.write_history_file = lambda *args: None
  '';

  xdg.configFile."ansible/ansible.cfg".text = ''
    [defaults]
    remote_tmp=/home/snake/.config/ansible/tmp
    interpreter_python=auto_silent
  '';

  xdg.configFile."libvirt/libvirt.conf".text = ''
    uri_default = "qemu:///system"
  '';

  programs.npm = {
    enable = true;
    package = null;
    settings = {
      prefix = "${config.xdg.dataHome}/npm";
      cache = "${config.xdg.cacheHome}/npm";
      init-module = "${config.xdg.configHome}/npm/config/npm-init.js";
      logs-dir = "${config.xdg.stateHome}/npm/logs";
    };
  };

  xdg.configFile."pnpm/rc".text = ''
    auto-install-peers=true
    ignore-scripts=false
    dangerouslyAllowAllBuilds=true
  '';

  xdg.configFile."nimble/nimble.ini".text = ''
    nimbleDir = ~/.local/share/nimble
  '';

  xdg.configFile."xdg-desktop-portal/portals.conf".text = ''
    [preferred]
    default=gtk
    org.freedesktop.impl.portal.FileChooser=gtk
  '';
  xdg.configFile."jupyter/jupyter_notebook_config.py".text = ''
    c = get_config()
    c.FileContentsManager.checkpoints_class =
        "notebook.services.contents.checkpoints.NoOpCheckpoints"
  '';
}
