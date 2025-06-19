{ config, pkgs, lib, ... }:
{
  #bridge
  config = lib.mkIf config.services.trezord.enable {
    environment.systemPackages = with pkgs; [
      trezorctl
      trezor-suite
    ];

    services.udev.packages = [ pkgs.trezor-udev-rules ];
  };
}
