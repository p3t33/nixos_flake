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
        vm_server = lib.nixosSystem {
	  inherit system;
	  modules = [ 
	  ./hosts/vm_server/configuration.nix 
	  home-manager.nixosModules.home-manager
	  {
	    home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
	    home-manager.users.drone = import ./hosts/vm_server/home.nix;
	  }
	  ];
	};
      };

      nixosConfigurations = {
        vm_gui = lib.nixosSystem {
	  inherit system;
	  modules = [ 
	  ./hosts/vm_gui/configuration.nix 
	  home-manager.nixosModules.home-manager
	  {
	    home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
	    home-manager.users.kmedrish = import ./hosts/vm_gui/home.nix;
	  }
	  ];
	};
      };

      nixosConfigurations = {
        work_pc = lib.nixosSystem {
	  inherit system;
	  modules = [ 
	  ./hosts/work_pc/configuration.nix 
	  home-manager.nixosModules.home-manager
	  {
	    home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
	    home-manager.users.kmedrish = import ./hosts/work_pc/home.nix;
	  }
	  ];
	};
      };

      nixosConfigurations = {
        home_desktop = lib.nixosSystem {
	  inherit system;
	  modules = [ 
	  ./hosts/home_desktop/configuration.nix 
	  home-manager.nixosModules.home-manager
	  {
	    home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
	    home-manager.users.kmedrish = import ./hosts/home_desktop/home.nix;
	  }
	  ];
	};
      };

    hmConfig = {
      drone = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
	modules = [
	  ./home/vm/home.nix
	];
      };
    };
    };
}
