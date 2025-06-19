{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./services-configuration.nix
    ./sops-configuration.nix
    ./disko-configuration.nix
    ../../modules/nixos # imported via default.nix
  ];

  custom = {
    profiles.system = {
      core = true;
      desktop = true;
      securityKeys = true;
      virtualization = true;
    };

    networking = {
      bridgedInterface.enable = true;
      usb0StaticIp.enable = true;
    };

    systemStateVersion = "24.05";
  };

  # As the intel GPU in this machine is too new to be officially supported by the i915 driver
  # it needs to be forced, if this is not done the main gpu won't work and this will cause bunch
  # of bad things, including the external monitors connected via the dock not to work.
  boot.kernelParams = [
    "i915.force_probe=7d55"
  ];

  environment.systemPackages = with pkgs; [
    moolticute
    syncthing
    git-review # cli tool to interact with gerrit.
    vim
  ];
}
