{ config, lib, ... }:
{
  # Used for CUPS to automatically discover IPP printers.
  config = lib.mkIf config.services.avahi.enable {
    services.avahi = {
      nssmdns4 = true;
      # for a WiFi printer
      openFirewall = true;
    };
  };
}
