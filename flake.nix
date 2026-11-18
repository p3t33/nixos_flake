{
  description = "Multi machine flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    import-tree.url = "github:denful/import-tree";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
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

    hermes-agent = {
      url = "github:nousresearch/hermes-agent";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    tmux-nerd-font-window-name = {
      url = "github:joshmedeski/tmux-nerd-font-window-name";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      # Review whether this version override is still needed on every NixOS stable upgrade.
      nomachineVersionFix = final: prev: {
        nomachine-client = prev.nomachine-client.overrideAttrs (old: {
          version = "10.1.7";
          src = prev.fetchurl {
            url = "https://download.nomachine.com/download/10.1/Linux/nomachine-enterprise-client_10.1.7_1_x86_64.tar.gz";
            hash = "sha256-qvScbUV1Tt7LxYgrNpcdsRjjAippdZ/L1nrteqekC8Q=";
          };
        });
      };
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ nomachineVersionFix ];
      };

      pkgs-unstable = import inputs.nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };

      lib = nixpkgs.lib;

      # Discover NixOS machines by directory name.
      # Only directories containing configuration.nix are included.
      machinesByDirectoryName = lib.filterAttrs
        (name: type:
          type == "directory"
          && builtins.pathExists ./machines/${name}/configuration.nix
        )
        (builtins.readDir ./machines);

      # one of the machines has different username which
      # is stated here, this section is used to change
      # specific values for specific machines.
      machineOverrides = {
        kvm-nixos-server = {
          primeUsername = "drone";
        };
      };

      # function that applies common logic for all of the machines.
      mkHost = { hostName, primeUsername ? "kmedrish" }: lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit inputs pkgs-unstable;
          hostSpecific = { inherit hostName primeUsername; };
        };
        modules = [
          ./machines/${hostName}/configuration.nix
          {
            nixpkgs.overlays = [ nomachineVersionFix ];
          }
          inputs.home-manager.nixosModules.home-manager
          inputs.nix-index-database.nixosModules.nix-index
          inputs.sops-nix.nixosModules.sops
          inputs.disko.nixosModules.disko
          inputs.hermes-agent.nixosModules.default
        ];
      };
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.flake-parts.flakeModules.modules
        (inputs.import-tree ./modules)
      ];
      systems = [ system ];

      flake = {
        nixosConfigurations = lib.mapAttrs
          (machineName: _:
            mkHost (
              (machineOverrides.${machineName} or {})
              // { hostName = machineName; }
            )
          )
          machinesByDirectoryName;

        # Created for the case I would like to apply home manger settings on non NixOS
        # OS such as Lubuntu.
        # For this to work:
        # The username on the existing system most match the one defined in homeConfigurations.
        homeConfigurations = {
          kmedrish = inputs.home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = {
              inherit inputs pkgs-unstable;
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
    };
}
