{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  };
  outputs = { self, nixpkgs }: 
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
	  modules = [ ./configuration.nix ];
	};
      };
    };
}
