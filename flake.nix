{
    description = "midori's nixos config flake";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-23.05";
        home-manager = {
            url = "github:nix-community/home-manager/release-23.05";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        nixvim = {
            url = "github:nix-community/nixvim/nixos-23.05";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        musnix = { url = "github:musnix/musnix"; };
    };

    outputs = { nixpkgs, home-manager, nixvim, musnix, ... }: 
    let
        lib = nixpkgs.lib;
        system = "x86_64-linux";
        pkgs = import nixpkgs {
            inherit system;
            config = { allowUnfree = true; };
        };
    in {
        nixosConfigurations = {
            nixos = lib.nixosSystem {
                inherit system;

                modules = [
                    ./system/configuration.nix
                    ./system/hosts/desktop.nix
                    musnix.nixosModules.musnix
                ];
            };
        };
        homeManagerConfigurations = {
            midori = home-manager.lib.homeManagerConfiguration {
                inherit pkgs;

                modules = [
                    ./users/home.nix 
                    nixvim.homeManagerModules.nixvim
                ];
            };
        };
    };
}
