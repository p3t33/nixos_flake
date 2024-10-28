{ ... }:
{
  # Used for CUPS to automatically discover IPP printers.
  services.avahi.enable = true;
  services.avahi.nssmdns4 = true;
  # for a WiFi printer
  services.avahi.openFirewall = true;
}
