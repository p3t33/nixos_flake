{ config, pkgs, lib, ... }:

let
  cfg = config.customOptions.enableModule.trezor;
in
{
  options.customOptions.enableModule.trezor = lib.mkEnableOption "Enable Trezor support (bridge, udev rules, tools)";

  config = lib.mkIf cfg {
    #bridge
    services.trezord.enable = true;

    environment.systemPackages = with pkgs; [
      trezorctl
      trezor-suite
    ];

    services.udev.packages = [ pkgs.trezor-udev-rules ];
  };
}
