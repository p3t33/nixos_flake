{ config, pkgs, ... }:
{
    #bridge
    services.trezord.enable = true;

    environment.systemPackages = with pkgs; [
        trezorctl
        trezor-suite
    ];

    services.udev.packages = [
        pkgs.trezor-udev-rules
    ];
}


