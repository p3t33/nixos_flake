{
    description = "Multi machine flake";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

        home-manager = {
            url = "github:nix-community/home-manager/release-24.05";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        nix-index-database = {
            url = "github:Mic92/nix-index-database";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        sops-nix = {
            url = "github:Mic92/sops-nix";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        disko = {

            url = "github:nix-community/disko";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };


    outputs = { nixpkgs, ... }@inputs:
    let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
        };

        lib = nixpkgs.lib;
    in
    {
        nixosConfigurations = {
            vm_server = lib.nixosSystem {
                inherit system;
                specialArgs = { inherit inputs; machineName = "kvm-nixos-server"; };
                modules = [
                    ./hosts/vm_server/configuration.nix
                    inputs.home-manager.nixosModules.home-manager
                    inputs.nix-index-database.nixosModules.nix-index
                    inputs.sops-nix.nixosModules.sops
                    inputs.disko.nixosModules.disko
                ];
            };
            homelab = lib.nixosSystem {
                inherit system;
                specialArgs = { inherit inputs; machineName = "homelab"; };
                modules = [
                    ./hosts/homelab/configuration.nix
                    inputs.home-manager.nixosModules.home-manager
                    inputs.nix-index-database.nixosModules.nix-index
                    inputs.sops-nix.nixosModules.sops
                    inputs.disko.nixosModules.disko
                ];
            };

            vm_gui = lib.nixosSystem {
                inherit system;
                specialArgs = { inherit inputs; machineName = "kvm-nixos-gui"; };
                modules = [
                    ./hosts/vm_gui/configuration.nix
                    inputs.home-manager.nixosModules.home-manager
                    inputs.nix-index-database.nixosModules.nix-index
                    inputs.sops-nix.nixosModules.sops
                    inputs.disko.nixosModules.disko
                ];
            };

            work_pc = lib.nixosSystem {
                inherit system;
                specialArgs = { inherit inputs; machineName = "HP-Zbook"; };
                modules = [
                    ./hosts/work_pc/configuration.nix
                    inputs.home-manager.nixosModules.home-manager
                    inputs.nix-index-database.nixosModules.nix-index
                    inputs.sops-nix.nixosModules.sops
                ];
            };

            home_desktop = lib.nixosSystem {
                inherit system;
                specialArgs = { inherit inputs; machineName = "home-desktop"; };
                modules = [
                    ./hosts/home_desktop/configuration.nix
                    inputs.home-manager.nixosModules.home-manager
                    inputs.nix-index-database.nixosModules.nix-index
                    inputs.sops-nix.nixosModules.sops
                ];
            };
        };

        # Created for the case I would like to apply home manger settings on non NixOS
        # OS such as Lubuntu.
        # For this to work:
        # The username on the existing system most match the one defined in homeConfigurations.
        homeConfigurations = {
            kmedrish = inputs.home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                extraSpecialArgs = {inherit inputs; machineName = "generic_linux_distro";};
                modules = [
                    ./hosts/generic_linux_distro/home.nix
                    inputs.sops-nix.homeManagerModules.sops
                ];
            };
        };
    };
}
