{ config, lib, ... }:

let
  # Define your network interfaces here
  networkInterfaces = [
    { nameToSetWithUdev = "usbeth0"; macAddress = "00:e0:4c:5e:59:c8"; }
    { nameToSetWithUdev = "alpha-sniffer"; macAddress = "00:c0:ca:b4:af:f4"; }
    # Add more interfaces here:
    # { nameToSetWithUdev = "eth0"; macAddress = "aa:bb:cc:dd:ee:ff"; }
  ];

  # Import the interface module for each interface
  interfaceImports = builtins.map (iface: import ./interface.nix {
    inherit config lib;
    nameToSetWithUdev = iface.nameToSetWithUdev;
    macAddress = iface.macAddress;
  }) networkInterfaces;
in
{
  imports = interfaceImports;
}
