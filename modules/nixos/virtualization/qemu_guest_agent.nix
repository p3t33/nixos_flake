{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.qemuGuest;
in
{
  options.customOptions.enableModule.qemuGuest = lib.mkEnableOption "Enable QEMU Guest services (intended for use in NixOS VMs)";

  # Only intended for the use of NixOS that runs as a VM(guest).
  config = lib.mkIf cfg {
    services.qemuGuest.enable = true;
  };
}
