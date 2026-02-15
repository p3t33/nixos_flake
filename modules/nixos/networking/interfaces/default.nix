{ config, lib, ... }:

let
  # Define your network interfaces here.
  # Each entry must have nameToSetWithUdev and either:
  #   - macAddress: for devices with a stable MAC address
  #   - usbVendorId + usbProductId: for USB devices with changing MAC addresses
  #   - optionally usbSerial: to distinguish between multiple identical USB devices
  networkInterfaces = [
    { nameToSetWithUdev = "usbeth0"; macAddress = "00:e0:4c:5e:59:c8"; }
    { nameToSetWithUdev = "alpha-sniffer"; macAddress = "00:c0:ca:b4:af:f4"; }
    { nameToSetWithUdev = "svx"; usbVendorId = "0cad"; usbProductId = "150d"; }
  ];

  # Import the interface module for each interface
  interfaceImports = builtins.map (iface: import ./interface.nix {
    inherit config lib;
    nameToSetWithUdev = iface.nameToSetWithUdev;
    macAddress = iface.macAddress or null;
    usbVendorId = iface.usbVendorId or null;
    usbProductId = iface.usbProductId or null;
    usbSerial = iface.usbSerial or null;
  }) networkInterfaces;
in
{
  imports = interfaceImports;
}
