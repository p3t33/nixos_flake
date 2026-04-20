{
  description = "Multi machine flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

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

    nix-openclaw = {
      url = "github:openclaw/nix-openclaw";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tmux-nerd-font-window-name = {
      url = "github:joshmedeski/tmux-nerd-font-window-name";
      flake = false;
    };

    whisper-model-base = {
      url = "file+https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.bin";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      nomachineHashFix = final: prev: {
        nomachine-client = prev.nomachine-client.overrideAttrs (old: {
          src = prev.fetchurl {
            url = "https://download.nomachine.com/download/9.4/Linux/nomachine_9.4.14_1_x86_64.tar.gz";
            hash = "sha256-tLL8l/UgTiVzGs+mwJeRUlVA8lH72JVogBOEpaSr2AY=";
          };
        });
      };
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ inputs.nix-openclaw.overlays.default nomachineHashFix ];
      };
      pkgs-unstable = import inputs.nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
        overlays = [ inputs.nix-openclaw.overlays.default ];
      };

      lib = nixpkgs.lib;

      mkHost = { hostName, primeUsername ? "kmedrish" }: lib.nixosSystem {
        inherit system;
        specialArgs = {
          inherit inputs pkgs-unstable;
          hostSpecific = { inherit hostName primeUsername; };
        };
        modules = [
          ./machines/${hostName}/configuration.nix
          {
            nixpkgs.overlays = [ inputs.nix-openclaw.overlays.default nomachineHashFix ];
          }
          inputs.home-manager.nixosModules.home-manager
          inputs.nix-index-database.nixosModules.nix-index
          inputs.sops-nix.nixosModules.sops
          inputs.disko.nixosModules.disko
        ];
      };
    in
    {
      nixosConfigurations = {
        kvm-nixos-server = mkHost { hostName = "kvm-nixos-server"; primeUsername = "drone"; };
        nas              = mkHost { hostName = "nas"; };
        home-assistant   = mkHost { hostName = "home-assistant"; };
        sisyphus-miner   = mkHost { hostName = "sisyphus-miner"; };
        work-pc          = mkHost { hostName = "work-pc"; };
        home-desktop     = mkHost { hostName = "home-desktop"; };
        helper           = mkHost { hostName = "helper"; };
      };

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
}
