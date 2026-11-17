{ ... }:
{
  flake.modules.nixos.profile-system-services = { config, lib, inputs, ... }:

let
  g = config.custom.profiles.systemServices;
in
{
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

  config = lib.mkMerge [
    (lib.mkIf g.core.enable {
      custom.services.tmuxd.enable = true;
      custom.services.mutagen.enable = true;
      custom.services.watchman.enable = true;
    })

    (lib.mkIf g.desktop.common.enable {
      custom.services.tmuxd.startWithGraphicalSession = true;
      services.avahi.enable = true;
      custom.services.printing.client.enable = true;
      services.udisks2.enable = true; # mount and unmout USB drives
      services.pipewire.enable = true; # sound
      services.gnome.gnome-keyring.enable = true;
      services.upower.enable = true;
      custom.services.moolticuted.enable = true;
    })

    (lib.mkIf g.desktop.wayland.enable {
      programs.sway.enable = true;
    })

    (lib.mkIf g.server.enable {
      services.openssh.enable  = true; # sshd
      services.fail2ban.enable = true;
    })

    (lib.mkIf g.xmr-miner.enable {
      services.monero.enable = true;
      custom.services.p2pool.enable = true;
      services.xmrig.enable = true;
    })

    (lib.mkIf g.monitoring.enable {
      services.prometheus.enable = true;
      services.loki.enable = true;
      services.alloy.enable = true;
      services.grafana.enable = true;
    })

    (lib.mkIf g.wireguardServer.enable {
      custom.vpn.wireguardServer.enable = true;
      # as inadyn is no longer maintained(and so has been deleted) if
      # this profile to be enabled a new dynamic dns service is required to
      # be configured.
    })
  ];
};
}
