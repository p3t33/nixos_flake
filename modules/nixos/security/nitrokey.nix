{ pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
        pynitrokey
    ];

    services.udev.packages = [ pkgs.nitrokey-udev-rules];
}
