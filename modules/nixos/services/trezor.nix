{ config, pkgs, lib, ... }:
{
  #bridge
  config = lib.mkIf config.services.trezord.enable {
    environment.systemPackages = with pkgs; [
      # trezorctl # uses non safe python3.13-ecdsa-0.19.1
      trezor-suite
    ];

    services.udev.packages = [ pkgs.trezor-udev-rules ];
  };
}
