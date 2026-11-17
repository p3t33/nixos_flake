{ ... }:
{
  flake.modules.nixos.profile-system = { config, lib, pkgs, inputs, ... }:

let
  g = config.custom.profiles.system;
in
{
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

  config = lib.mkMerge [
    (lib.mkIf g.core.enable {
      custom.apps.development.enable  = true;
      custom.apps.cliUtilities.enable = true;
      custom.apps.encryption.enable   = true;
      custom.networking.disableLegacyBridgeNetfilter.enable = true;
      programs.nix-ld.enable = true;
    })

    (lib.mkIf g.desktop.enable {
      custom.apps.desktopEnvironment.enable = true;
      custom.apps.chrome.enable = true;
      custom.apps.gui.enable = true;
      programs.dconf.enable = true;
    })

    (lib.mkIf g.server.enable {
      virtualisation.docker.enable = true;
      custom.motd.enable = true; # moto of the day.
      environment.systemPackages = [ pkgs.ghostty.terminfo ];
    })

    # virtualbox is not enbled by default as it is mostly used for cross development.
    (lib.mkIf g.virtualization.enable {
     virtualisation.docker.enable = true;
      virtualisation.libvirtd.enable = true;
      virtualisation.incus.enable = true; # sets nftables to true.
    })

    (lib.mkIf g.securityKeys.enable {
      custom.security.nitrokey.enable = true;
      custom.security.solokey2.enable = true;
    })

    (lib.mkIf g.gaming.enable {
      programs.steam.enable = true;
    })
  ];
};
}
