{ lib, ... }:
{
   # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
   # (the default) this is the recommended approach. When using systemd-networkd it's
   # still possible to use this option, but it's recommended to use it in conjunction
   # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
   networking.useDHCP = lib.mkDefault true;
   # networking.interfaces.enp0s20f0u6u4.useDHCP = lib.mkDefault true;
   # networking.interfaces.wlp0s20f3.useDHCP = lib.mkDefault true;
}
