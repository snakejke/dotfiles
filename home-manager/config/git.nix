{ config, pkgs, ... }:
let
  testGitDir = "${config.home.homeDirectory}/Documents/Projects/05_Git";
in
{
  programs.git = {
    enable = true;
    
    userName  = "Artem Bliznetsov";
    userEmail = "snakejke@proton.me";
    
    includes = [{
      condition = "gitdir:${testGitDir}/";
      contents.user.name  = "brutal-force";
      contents.user.email = "snake05865@gmail.com";
      contents.user.signingkey = "ABCDEF1234567890";
    }];

    extraConfig = {
      core = {
        quotepath = false;
        pager = "delta";
      };
      interactive = {
        diffFilter = "delta --color-only";
      };
      delta = {
        navigate = true;
        dark = true;
        line-numbers = true;
        side-by-side = true;
      };
      merge = {
        conflictStyle = "zdiff3";
      };
      github = {
        user = "snakejke";
      };
      gpg = {
        format = "openpgp";
        openpgp = {
          program = "/nix/store/m7b03yjgsang1kws4lwhzk0dr0dkrjwf-gnupg-2.4.8/bin/gpg";
        };
      };
    };
  };
}
