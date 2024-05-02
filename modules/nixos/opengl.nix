{ config, machineName, pkgs, ... }:
{
    hardware.opengl = {
        enable = true;
        driSupport = true;
        driSupport32Bit = true;
    };
}
