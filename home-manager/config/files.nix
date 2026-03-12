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

  xdg.configFile."npm/npmrc".text = ''
    prefix=''${XDG_DATA_HOME}/npm
    cache=''${XDG_CACHE_HOME}/npm
    init-module=''${XDG_CONFIG_HOME}/npm/config/npm-init.js
    logs-dir=''${XDG_STATE_HOME}/npm/logs
  '';

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
