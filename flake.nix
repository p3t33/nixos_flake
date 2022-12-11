{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };


  outputs = inputs@{ nixpkgs, home-manager, ... }: 
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
	config.allowUnfree = true;
      };

      lib = nixpkgs.lib;
    in{
      nixosConfigurations = {
        kvm-nixos = lib.nixosSystem {
	  inherit system;
	  modules = [ 
	  ./configuration.nix 
	  home-manager.nixosModules.home-manager
	  {
	    home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
	    home-manager.users.drone = import ./vm_home.nix;
	  }
	  ];
	};
      };

    hmConfig = {
      drone = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
	modules = [
	  ./home.nix
	];
      };
    };
    };
}
