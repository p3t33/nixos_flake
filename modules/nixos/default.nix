{ config, lib, ... }:

let
  g = config.custom.profiles.system;
in
{
  imports = [
    ../custom-global-options.nix # used both for nixos and home-manager
    ./auto_upgrade.nix
    ./dconf.nix
    ./defaults_for_system_build.nix
    ./desktop_environment.nix
    ./dictionaries.nix
    ./environment_variables.nix
    ./experimental-features.nix
    ./fonts.nix
    ./garbage_collection.nix
    ./graphics.nix
    ./home-manager-as-nixos-module.nix
    ./locale.nix
    ./motd.nix
    ./non_free_software.nix
    ./system_version.nix
    ./users.nix

    ./system_packages/cli_utilities.nix
    ./system_packages/development.nix
    ./system_packages/encryption.nix
    ./system_packages/gui.nix

    ./bootloader/systemd-boot.nix

    ./virtualization/docker.nix
    ./virtualization/kvm.nix
    ./virtualization/virtualbox.nix

    ./networking/networkmanager.nix
    ./networking/hostname.nix
    ./networking/br0_interface.nix
    ./networking/usb0.nix

    ./security/nitrokey.nix
    ./security/solokey2.nix

    ./gaming/steam.nix

    ./wireshark.nix
  ];

  options.custom.profiles.system = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.enable = lib.mkEnableOption "Enable this system profile.";
    });
    default = {};
    description = "Enable system profiles like 'desktop', 'server', 'securityKeys', etc.";
  };

  config = lib.mkMerge [
    (lib.mkIf (g.core.enable or false) {
      custom.apps.development.enable  = true;
      custom.apps.cliUtilities.enable = true;
      custom.apps.encryption.enable   = true;
    })

    (lib.mkIf (g.desktop.enable or false) {
      custom.apps.desktopEnvironment.enable = true;
      custom.apps.gui.enable = true;
      programs.dconf.enable = true;
    })

    (lib.mkIf (g.server.enable or false) {
      virtualisation.docker.enable = true;
      custom.motd.enable = true; # moto of the day.
    })

    # virtualbox is not enbled by default as it is mostly used for cross development.
    (lib.mkIf (g.virtualization.enable or false) {
     virtualisation.docker.enable = true;
      virtualisation.libvirtd.enable = true;
    })

    (lib.mkIf (g.securityKeys.enable or false) {
      custom.security.nitrokey.enable = true;
      custom.security.solokey2.enable = true;
    })

    (lib.mkIf (g.gaming.enable or false) {
      programs.steam.enable = true;
    })
  ];
}
