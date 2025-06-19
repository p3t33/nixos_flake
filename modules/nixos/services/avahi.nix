{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.avahi;
in
{
  options.customOptions.enableModule.avahi = lib.mkEnableOption "Enable Avahi for network service discovery";

  config = lib.mkIf cfg {
    # Used for CUPS to automatically discover IPP printers.
    services.avahi.enable = true;
    services.avahi.nssmdns4 = true;
    # for a WiFi printer
    services.avahi.openFirewall = true;
  };
}
