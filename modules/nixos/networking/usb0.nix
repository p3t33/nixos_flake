{
  networking.networkmanager.ensureProfiles.profiles.usb0-static = {

    connection = {
      id              = "usb0-static";
      type            = "ethernet";
      "interface-name" = "usb0";
      autoconnect     = true;
    };

    ipv4 = {
      address1 = "192.168.99.1/24";
      method   = "manual"; # static ip
    };

    ipv6.method = "ignore";
  };
}

