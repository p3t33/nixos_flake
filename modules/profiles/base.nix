{ ... }:
{
  flake.modules.nixos.profile-system = { lib, inputs, ... }: {
    imports = [
      inputs.self.modules.generic.shared-options # used both for nixos and home-manager
      inputs.self.modules.nixos.networking-options
      inputs.self.modules.nixos.lan-hosts
      inputs.self.modules.nixos.auto-upgrade
      inputs.self.modules.nixos.chrome
      inputs.self.modules.nixos.dconf
      inputs.self.modules.nixos.system-build-defaults
      inputs.self.modules.nixos.desktop-environment
      inputs.self.modules.nixos.dictionaries
      inputs.self.modules.nixos.environment-variables
      inputs.self.modules.nixos.experimental-features
      inputs.self.modules.nixos.fonts
      inputs.self.modules.nixos.garbage-collection
      inputs.self.modules.nixos.graphics
      inputs.self.modules.nixos.home-manager-integration
      inputs.self.modules.nixos.locale
      inputs.self.modules.nixos.motd
      inputs.self.modules.nixos.non-free-software
      inputs.self.modules.nixos.system-version
      inputs.self.modules.nixos.users

      inputs.self.modules.nixos.packages-cli-utilities
      inputs.self.modules.nixos.packages-development
      inputs.self.modules.nixos.packages-encryption
      inputs.self.modules.nixos.packages-gui

      inputs.self.modules.nixos.systemd-boot

      inputs.self.modules.nixos.docker
      inputs.self.modules.nixos.kvm
      inputs.self.modules.nixos.incus
      inputs.self.modules.nixos.virtualbox

      inputs.self.modules.nixos.networkmanager
      inputs.self.modules.nixos.hostname
      inputs.self.modules.nixos.bridge-br0
      inputs.self.modules.nixos.bridge-netfilter
      inputs.self.modules.nixos.network-interfaces

      inputs.self.modules.nixos.profile-system-services
      inputs.self.modules.nixos.hardware

      inputs.self.modules.nixos.nitrokey
      inputs.self.modules.nixos.solokey2

      inputs.self.modules.nixos.steam

      inputs.self.modules.nixos.wireshark
    ];

    options.custom.profiles.system = {
      core.enable = lib.mkEnableOption "core system profile (dev tools, CLI utilities, encryption)";
      desktop.enable = lib.mkEnableOption "desktop system profile (DE, GUI apps, dconf)";
      server.enable = lib.mkEnableOption "server system profile (docker, MOTD)";
      virtualization.enable = lib.mkEnableOption "virtualization system profile (docker, KVM, Incus)";
      securityKeys.enable = lib.mkEnableOption "security keys system profile (Nitrokey, SoloKey2)";
      gaming.enable = lib.mkEnableOption "gaming system profile (Steam)";
    };
  };

  flake.modules.nixos.profile-system-services = { lib, inputs, ... }: {
    imports = [
      inputs.self.modules.nixos.adguard-home
      inputs.self.modules.nixos.acme
      inputs.self.modules.nixos.avahi
      inputs.self.modules.nixos.bluetooth
      inputs.self.modules.nixos.calibre-web
      inputs.self.modules.nixos.envfs # always enabled
      inputs.self.modules.nixos.nix-ld
      inputs.self.modules.nixos.fail2ban
      inputs.self.modules.nixos.gatus
      inputs.self.modules.nixos.grafana
      inputs.self.modules.nixos.home-assistant
      inputs.self.modules.nixos.homepage-dashboard
      inputs.self.modules.nixos.download-clients
      inputs.self.modules.nixos.media
      inputs.self.modules.nixos.loki
      inputs.self.modules.nixos.mosquitto
      inputs.self.modules.nixos.nginx
      inputs.self.modules.nixos.paperless
      inputs.self.modules.nixos.n8n
      inputs.self.modules.nixos.printing
      inputs.self.modules.nixos.prometheus
      inputs.self.modules.nixos.alloy
      inputs.self.modules.nixos.restic
      inputs.self.modules.nixos.samba
      inputs.self.modules.nixos.sftp-backup
      inputs.self.modules.nixos.sound
      inputs.self.modules.nixos.sshd
      inputs.self.modules.nixos.trezor
      inputs.self.modules.nixos.zigbee2mqtt

      inputs.self.modules.nixos.gnome-keyring
      inputs.self.modules.nixos.sway
      inputs.self.modules.nixos.greetd

      inputs.self.modules.nixos.moolticute

      inputs.self.modules.nixos.tmux
      inputs.self.modules.nixos.mutagen
      inputs.self.modules.nixos.watchman

      inputs.self.modules.nixos.wg-quick-client
      inputs.self.modules.nixos.wireguard-server

      inputs.self.modules.nixos.zfs-auto-scrub

      inputs.self.modules.nixos.syncthing
      inputs.self.modules.nixos.postgresql
      inputs.self.modules.nixos.postgresql-backup
      inputs.self.modules.nixos.gvfs

      # mining
      inputs.self.modules.nixos.monerod
      inputs.self.modules.nixos.p2pool
      inputs.self.modules.nixos.xmrig

      inputs.self.modules.nixos.immich
      inputs.self.modules.nixos.ollama
      inputs.self.modules.nixos.minecraft
      inputs.self.modules.nixos.hermes-agent
    ];

    options.custom.profiles.systemServices = {
      core.enable = lib.mkEnableOption "core service profile (tmux daemon, watchman)";
      desktop = {
        common.enable = lib.mkEnableOption "common desktop service profile";
        wayland.enable = lib.mkEnableOption "Sway desktop service profile";
      };
      server.enable = lib.mkEnableOption "server service profile (sshd, fail2ban)";
      xmr-miner.enable = lib.mkEnableOption "XMR miner service profile (monerod, p2pool, xmrig)";
      wireguardServer.enable = lib.mkEnableOption "WireGuard server service profile (wireguard, dynamic dns)";
      monitoring.enable = lib.mkEnableOption "monitoring service profile (prometheus, loki, alloy, grafana)";
    };
  };

  flake.modules.homeManager.profile-home-manager = { lib, inputs, ... }: {
    # ignored for now
    # ./syncthing.nix used by stand alone home-manager.
    # ./generic_linux_fonts.nix used by stand alone home-manger.
    imports = [
      inputs.self.modules.homeManager.session-variables
      inputs.self.modules.homeManager.colors
      inputs.self.modules.generic.shared-options

      inputs.self.modules.homeManager.home-core
      inputs.self.modules.homeManager.starship
      inputs.self.modules.homeManager.fzf
      inputs.self.modules.homeManager.antigravity
      inputs.self.modules.homeManager.claude
      inputs.self.modules.homeManager.codex
      inputs.self.modules.homeManager.bat
      inputs.self.modules.homeManager.neovim
      inputs.self.modules.homeManager.git
      inputs.self.modules.homeManager.delta
      inputs.self.modules.homeManager.git-signing
      inputs.self.modules.homeManager.lazygit
      inputs.self.modules.homeManager.zoxide
      inputs.self.modules.homeManager.direnv
      inputs.self.modules.homeManager.zsh
      inputs.self.modules.homeManager.bash
      inputs.self.modules.homeManager.tmux
      inputs.self.modules.homeManager.alacritty
      inputs.self.modules.homeManager.wezterm
      inputs.self.modules.homeManager.flameshot
      inputs.self.modules.homeManager.sway
      inputs.self.modules.homeManager.waybar
      inputs.self.modules.homeManager.rofi
      inputs.self.modules.homeManager.gpg
      inputs.self.modules.homeManager.ssh-client
      inputs.self.modules.homeManager.ssh-public-key
      inputs.self.modules.homeManager.wlsunset
      inputs.self.modules.homeManager.taskwarrior
      inputs.self.modules.homeManager.atuin
      inputs.self.modules.homeManager.wallpaper
      inputs.self.modules.homeManager.xdg-mime-apps
      inputs.self.modules.homeManager.xdg-user-dirs
      inputs.self.modules.homeManager.gtk
      inputs.self.modules.homeManager.qt
      inputs.self.modules.homeManager.moolticute
      inputs.self.modules.homeManager.glabels
      inputs.self.modules.homeManager.firefox
      inputs.self.modules.homeManager.whatsapp
      inputs.self.modules.homeManager.zellij
      inputs.self.modules.homeManager.yazi
      inputs.self.modules.homeManager.ghostty
      inputs.self.modules.homeManager.cliphist
      inputs.self.modules.homeManager.dunst
      inputs.self.modules.homeManager.mcp-gateway
      inputs.self.modules.homeManager.swayidle
      inputs.self.modules.homeManager.navi
      inputs.self.modules.homeManager.tealdeer
      inputs.self.modules.homeManager.pay-respects
      inputs.self.modules.homeManager.zathura
      inputs.self.modules.homeManager.lnav

      # scripts
      inputs.self.modules.homeManager.display-recovery
      inputs.self.modules.homeManager.tmux-sessionizer
      inputs.self.modules.homeManager.helloworld-python
      inputs.self.modules.homeManager.cheat-sh

      inputs.self.modules.homeManager.emacs
      inputs.self.modules.homeManager.mcp
      inputs.self.modules.homeManager.pi
    ];

    options.custom.profiles.homeManager = {
      core.enable = lib.mkEnableOption "core home-manager profile (shell, terminal, editor, git, CLI tools)";
      desktop = {
        common.enable = lib.mkEnableOption "common desktop home-manager profile";
        wayland.enable = lib.mkEnableOption "Sway desktop home-manager profile";
      };
    };
  };
}
