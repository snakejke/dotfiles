{
  config,
  lib,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    podman-tui
    podman-compose
  ];
  
  services.podman = {
    enable = true;

    settings.storage = {
      storage = {
        driver = "overlay";
        runroot = "/run/containers/storage";
        rootless_storage_path = "$HOME/.local/share/containers/storage";

        options = {
          additionalimagestores = [];
          mountopt = "nodev";

          pull_options = {
            enable_partial_images = "true";
            use_hard_links = "false";
            ostree_repos = "";
          };
        };
      };
    };

    settings.registries = {
      search = [ "docker.io" "ghcr.io" "quay.io" ];
      insecure = [];
      block = [];
    };

    settings.policy = {
      default = [{ type = "insecureAcceptAnything"; }];
      transports = {
        docker-daemon."" = [{ type = "insecureAcceptAnything"; }];
      };
    };

    settings.containers = {
      engine = {
        cgroup_manager = "cgroupfs";
        events_logger = "file";
        runtime = "crun";
      };
      network = {};
      containers = {};
    };
  };
}
