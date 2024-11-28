{ config, ... }:
{
  services.syncthing = {
    settings = {
        devices = {
            "kvm-nixos-server" = {
                id = "VMTWSEY-4TLKW4M-5KWF4NP-R44ZCUB-532E53K-WGO2YX3-GCRDHBV-3WGSHAI";
                autoAcceptFolders = true;
                name = "kvm-nixos-server";
                introducer = false;
            };
        };
    };
  };
}

