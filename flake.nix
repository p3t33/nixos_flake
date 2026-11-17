{
  description = "Multi machine flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

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
        ./features/tmux.nix
        ./features/moolticute.nix
        ./features/starship.nix
        ./features/fzf.nix
        ./features/zoxide.nix
        ./features/bat.nix
        ./features/direnv.nix
        ./features/tealdeer.nix
        ./features/pay-respects.nix
        ./features/delta.nix
        ./features/lazygit.nix
        ./features/yazi.nix
        ./features/alacritty.nix
        ./features/wezterm.nix
        ./features/zathura.nix
        ./features/firefox.nix
        ./features/glabels.nix
        ./features/whatsapp.nix
        ./features/lnav.nix
        ./features/gtk.nix
        ./features/qt.nix
        ./features/cheat-sh.nix
        ./features/shared-options.nix
        ./features/profiles/system.nix
        ./features/profiles/system-services.nix
        ./features/profiles/home-manager.nix
        ./features/home-manager-integration.nix
        ./features/atuin.nix
        ./features/bash.nix
        ./features/home-core.nix
        ./features/generic-linux-fonts.nix
        ./features/ghostty.nix
        ./features/git.nix
        ./features/git-signing.nix
        ./features/gpg.nix
        ./features/navi.nix
        ./features/display-recovery.nix
        ./features/helloworld-python.nix
        ./features/tmux-sessionizer.nix
        ./features/session-variables.nix
        ./features/colors.nix
        ./features/ssh-client.nix
        ./features/ssh-public-key.nix
        ./features/taskwarrior.nix
        ./features/zellij.nix
        ./features/zsh.nix
        ./features/syncthing/home-manager.nix
        ./features/neovim/default.nix
        ./features/emacs/home-manager.nix
        ./features/emacs/nixos.nix
        ./features/antigravity.nix
        ./features/claude.nix
        ./features/codex.nix
        ./features/mcp.nix
        ./features/mcp-gateway.nix
        ./features/pi/default.nix
        ./features/flameshot.nix
        ./features/waybar.nix
        ./features/wallpaper.nix
        ./features/wlsunset.nix
        ./features/cliphist.nix
        ./features/dunst.nix
        ./features/swayidle.nix
        ./features/xdg-mime-apps.nix
        ./features/xdg-user-dirs.nix
        ./features/sway/home-manager.nix
        ./features/rofi/default.nix
        ./features/hardware.nix
        ./features/cpu-microcode.nix
        ./features/dhcp-setup.nix
        ./features/host-platform.nix
        ./features/gpu-amd.nix
        ./features/gpu-amd-hardware-decoding.nix
        ./features/gpu-intel-hardware-decoding.nix
        ./features/gpu-nvidia.nix
        ./features/gpu-nvidia-hybrid-intel-offload.nix
        ./features/bridge-br0.nix
        ./features/bridge-netfilter.nix
        ./features/hostname.nix
        ./features/networkmanager.nix
        ./features/lan-hosts.nix
        ./features/networking-options.nix
        ./features/network-interfaces/default.nix
        ./features/auto-upgrade.nix
        ./features/systemd-boot.nix
        ./features/chrome.nix
        ./features/dconf.nix
        ./features/system-build-defaults.nix
        ./features/desktop-environment.nix
        ./features/dictionaries.nix
        ./features/environment-variables.nix
        ./features/experimental-features.nix
        ./features/fonts.nix
        ./features/steam.nix
        ./features/garbage-collection.nix
        ./features/graphics.nix
        ./features/locale.nix
        ./features/motd.nix
        ./features/non-free-software.nix
        ./features/nitrokey.nix
        ./features/solokey2.nix
        ./features/packages-cli-utilities.nix
        ./features/packages-development.nix
        ./features/packages-encryption.nix
        ./features/packages-gui.nix
        ./features/packages-iac.nix
        ./features/system-version.nix
        ./features/users.nix
        ./features/user-network-backup.nix
        ./features/user-primary.nix
        ./features/docker.nix
        ./features/incus.nix
        ./features/kvm.nix
        ./features/virtualbox.nix
        ./features/wireshark.nix
        ./features/download-clients/default.nix
        ./features/download-clients/torrent.nix
        ./features/download-clients/usenet.nix
        ./features/download-clients/deluge.nix
        ./features/download-clients/qbittorrent-nox.nix
        ./features/download-clients/sabnzbd.nix
        ./features/media/audiobookshelf.nix
        ./features/media/bazarr.nix
        ./features/media/jackett.nix
        ./features/media/jellyfin.nix
        ./features/media/kavita.nix
        ./features/media/prowlarr.nix
        ./features/media/radarr.nix
        ./features/media/recyclarr.nix
        ./features/media/sonarr.nix
        ./features/media/default.nix
        ./features/grafana/default.nix
        ./features/home-assistant/default.nix
        ./features/syncthing/nixos.nix
        ./features/acme.nix
        ./features/adguard-home.nix
        ./features/alloy.nix
        ./features/avahi.nix
        ./features/bluetooth.nix
        ./features/calibre-web.nix
        ./features/envfs.nix
        ./features/fail2ban.nix
        ./features/gatus.nix
        ./features/gnome-keyring.nix
        ./features/greetd.nix
        ./features/sway/nixos.nix
        ./features/gvfs.nix
        ./features/hermes-agent.nix
        ./features/homepage-dashboard.nix
        ./features/immich.nix
        ./features/loki.nix
        ./features/minecraft.nix
        ./features/monerod.nix
        ./features/mosquitto.nix
        ./features/n8n.nix
        ./features/nginx.nix
        ./features/nix-ld.nix
        ./features/ollama.nix
        ./features/p2pool.nix
        ./features/paperless.nix
        ./features/postgresql.nix
        ./features/postgresql-backup.nix
        ./features/printing.nix
        ./features/prometheus.nix
        ./features/restic.nix
        ./features/samba.nix
        ./features/sftp-backup.nix
        ./features/sound.nix
        ./features/sshd.nix
        ./features/trezor.nix
        ./features/mutagen.nix
        ./features/watchman.nix
        ./features/wg-quick-client.nix
        ./features/wireguard-server.nix
        ./features/xmrig.nix
        ./features/zfs-auto-scrub.nix
        ./features/zigbee2mqtt.nix
        ./features/sops/home-manager/sops-calibre.nix
        ./features/sops/home-manager/sops-common.nix
        ./features/sops/home-manager/sops-extra-ssh-hosts.nix
        ./features/sops/home-manager/sops-git-credentials.nix
        ./features/sops/home-manager/sops-mcp-gateway.nix
        ./features/sops/home-manager/sops-pi-brave-api-key.nix
        ./features/sops/home-manager/sops-ssh-development-keys-for-vm.nix
        ./features/sops/nixos/sops-common.nix
        ./features/sops/nixos/sops-networkmanager.nix
        ./features/sops/nixos/sops-prime-user-hashed-password.nix
        ./features/sops/nixos/sops-prowlarr.nix
      ];
      systems = [ system ];

      flake = {
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
    };
}
