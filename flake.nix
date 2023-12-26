{
    description = "Multi machine flake";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

        home-manager = {
            url = "github:nix-community/home-manager/release-23.11";
            inputs.nixpkgs.follows = "nixpkgs";
        };

        firefox-addons = {
            url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
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


    outputs = { nixpkgs, home-manager, ... }@inputs:
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
                specialArgs = { inherit inputs; };
                modules = [
                    ./hosts/vm_gui/configuration.nix
                    inputs.home-manager.nixosModules.home-manager
                    inputs.nix-index-database.nixosModules.nix-index
                    inputs.sops-nix.nixosModules.sops
                ];
            };

            work_pc = lib.nixosSystem {
                inherit system;
                specialArgs = { inherit inputs; };
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
        # 1. The host name on the existing system needs to match the one on ./hosts/work_pc/home.nix and ./meta/meta.nix
        # 2. The username on the existing system most match the one defined in homeConfigurations.
        homeConfigurations = {
            kmedrish = inputs.home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                extraSpecialArgs = {inherit inputs;}; #Needs to be tested
                modules = [
                    ./hosts/generic_linux_distro/home.nix
                    inputs.sops-nix.homeManagerModules.sops
                ];
            };
        };
    };
}
