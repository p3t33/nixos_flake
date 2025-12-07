{
  description = "Multi machine flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # implicitly sets programs.command-not-found.enable = false;
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

  outputs =
    { nixpkgs, ... }@inputs:
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
        kvm-nixos-server = lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs;
            hostSpecific = {
              hostName = "kvm-nixos-server";
              primeUsername = "drone";
            };
          };
          modules = [
            ./machines/kvm-nixos-server/configuration.nix
            inputs.home-manager.nixosModules.home-manager
            inputs.nix-index-database.nixosModules.nix-index
            inputs.sops-nix.nixosModules.sops
            inputs.disko.nixosModules.disko
          ];
        };

        nas = lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs;
            hostSpecific = {
              hostName = "nas";
              primeUsername = "kmedrish";
            };
          };
          modules = [
            ./machines/nas/configuration.nix
            inputs.home-manager.nixosModules.home-manager
            inputs.nix-index-database.nixosModules.nix-index
            inputs.sops-nix.nixosModules.sops
            inputs.disko.nixosModules.disko
          ];
        };

        home-assistant = lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs;
            hostSpecific = {
              hostName = "home-assistant";
              primeUsername = "kmedrish";
            };

          };
          modules = [
            ./machines/home-assistant/configuration.nix
            inputs.home-manager.nixosModules.home-manager
            inputs.nix-index-database.nixosModules.nix-index
            inputs.sops-nix.nixosModules.sops
            inputs.disko.nixosModules.disko
          ];
        };

        sisyphus-miner = lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs;
            hostSpecific = {
              hostName = "sisyphus-miner";
              primeUsername = "kmedrish";
            };

          };
          modules = [
            ./machines/sisyphus-miner/configuration.nix
            inputs.home-manager.nixosModules.home-manager
            inputs.nix-index-database.nixosModules.nix-index
            inputs.sops-nix.nixosModules.sops
            inputs.disko.nixosModules.disko
          ];
        };

        work-pc = lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs;
            hostSpecific = {
              hostName = "work-pc";
              primeUsername = "kmedrish";
            };
          };
          modules = [
            ./machines/work-pc/configuration.nix
            inputs.home-manager.nixosModules.home-manager
            inputs.nix-index-database.nixosModules.nix-index
            inputs.sops-nix.nixosModules.sops
            inputs.disko.nixosModules.disko
          ];
        };

        home-desktop = lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit inputs;
            hostSpecific = {
              hostName = "home-desktop";
              primeUsername = "kmedrish";
            };
          };
          modules = [
            ./machines/home-desktop/configuration.nix
            inputs.home-manager.nixosModules.home-manager
            inputs.nix-index-database.nixosModules.nix-index
            inputs.sops-nix.nixosModules.sops
            inputs.disko.nixosModules.disko
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
          extraSpecialArgs = {
            inherit inputs;
            hostSpecific = {
              hostName = "generic_linux_distro";
              primeUsername = "kmedrish";
            };
          };
          modules = [
            ./machines/generic_linux_distro/home.nix
            inputs.sops-nix.homeManagerModules.sops
          ];
        };
      };
    };
}
