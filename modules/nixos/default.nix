{ config, lib, ... }:

let
  g = config.customOptions.enableConfigurationProfile;
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

  # system packages
  ./system_packages/cli_utilities.nix
  ./system_packages/development.nix
  ./system_packages/encryption.nix
  ./system_packages/gui.nix

  # bootloader
  ./bootloader/systemd-boot.nix

  # virtualization
  ./virtualization/docker.nix
  ./virtualization/kvm.nix
  ./virtualization/qemu_guest_agent.nix

  # networking
  ./networking/networkmanager.nix
  ./networking/hostname.nix
  ./networking/br0_interface.nix
  ./networking/usb0.nix

  # security
  ./security/nitrokey.nix
  ./security/solokey2.nix

  # gaming
  ./gaming/steam.nix
  ];


    # Declare the option
    options.customOptions.enableConfigurationProfile = lib.mkOption {
      type = lib.types.attrsOf lib.types.bool;
      default = {};
      description = "Enable system profiles like 'desktop', 'server', 'securityKeys', etc.";
    };

     config.customOptions.enableModule = lib.mkMerge [

    # Core profile enables CLI, dev, encryption
    (lib.mkIf (g.core or false) {
      development  = true;
      cliUtilities = true;
      encryption   = true;
    })

    # Desktop profile enables GUI etc.
    (lib.mkIf (g.desktop or false) {
      desktopEnvironment = true;
      gui                = true;
      dconf              = true;
    })

    # Server profile enables minimal features
    (lib.mkIf (g.server or false) {
      docker = true;
      motd   = true;
    })

    # Virtualization tools
    (lib.mkIf (g.virtualization or false) {
      docker = true;
      kvm    = true;
    })

    # Security keys (Nitrokey, SoloKey)
    (lib.mkIf (g.securityKeys or false) {
      nitrokey = true;
      solokey2 = true;
    })

    # Gaming profile
    (lib.mkIf (g.gaming or false) {
      steamGaming = true;
    })
  ];
}
