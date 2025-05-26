{ config, pkgs, ... }:

{
    programs.git = {
      enable = true;
      userName  = "Artem Bliznetsov";
      userEmail = "snakejke@proton.me";

    	extraConfig = {
			  core = {
				  quotepath = false;
			};
        github = {
          user = "snakejke";
        };
		};
    };
    
}
