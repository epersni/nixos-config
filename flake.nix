{
  description = "My NixOS + Home Manager configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      nixosConfigurations = {
        desktop = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/desktop/configuration.nix
            ./hosts/desktop/hardware-configuration.nix
          ];
        };
      };

      homeConfigurations = {
        "nicklas@desktop" = home-manager.lib.homeManagerConfiguration {
          pkgs = pkgs;
          modules = [ (import ./home/nicklas/home.nix) ];
        };
      };
    };
}
