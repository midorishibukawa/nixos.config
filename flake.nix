{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
  home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, home-manager, ... }: 
  let
    lib = nixpkgs.lib;
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
    };


  in {
    nixosConfigurations = {
      midori-nixos = lib.nixosSystem {
        inherit system;

	modules = [
	  ({ pkgs, ... }: {
	    imports = [
	      ./system/configuration.nix
	      ./system/hosts/desktop.nix
	    ];
	  })
	];
      };
    };
  };
}
