{
  description = "Home Manager configuration of snake";
  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    st-flexipatch = {
      url = "path:./flakes/st-flexipatch";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/9e3a5b9a0c79e66b4a1e2490be606a058a6712fe";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # nixgl = {
    #   url = "github:guibou/nixGL/a8e1ce7d49a149ed70df676785b07f63288f53c5";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    
    # emacs = {
    #   url = "path:./flakes/emacs";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
  };
  # ,nixgl
  outputs =
    { nixpkgs, home-manager, st-flexipatch, emacs-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ emacs-overlay.overlays.default ];
      };
    in
    {
      homeConfigurations."snake" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          st-flexipatch-pkg = st-flexipatch.packages.${system}.default;
          # emacs-custom-pkg = emacs.packages.${system}.default;
          # nixgl = nixgl;
        };
        modules = [
          ./home.nix
          ./flakes
          ./modules
        ];
      };

      homeConfigurations."snake@wsl" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          st-flexipatch-pkg = st-flexipatch.packages.${system}.default;
        };
        modules = [
          ./home-wsl.nix
          ./flakes
          ./modules
        ];
      };
    };
}
