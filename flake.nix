{
    description = "Multi machine flake";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
        home-manager = {
            url = "github:nix-community/home-manager/release-23.11";
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
    in
    {
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

        # Created for the case I would like to apply home manger settings on non NixOS
        # OS such as Lubuntu.
        # For this to work:
        # 1. The host name on the existing system needs to match the one on ./hosts/work_pc/home.nix and ./meta/meta.nix
        # 2. The username on the existing system most match the one defined in homeConfigurations.
        homeConfigurations = {
            kmedrish = home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                modules = [
                    ./hosts/work_pc/home.nix
                ];
            };
        };
    };
}
