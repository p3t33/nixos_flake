{ config, lib, hostSpecific, ... }:
let
  cfg = config.custom.services.syncthing;
  syncthingDevices = [
    { name = "work-pc"; id = "Z47X7UP-AFRW6CM-UUWCAFF-A5BA6C4-PKEU7IR-XDHH6IE-N7JL54R-RTWLNAG"; }
    { name = "home-desktop"; id = "TQ34X45-BKERB7F-LBQSEGR-ZAGQITL-RL5B242-PQSTHCX-2XBQBYL-ORMFTAH"; }
    { name = "homelab"; id = "XPCO572-XPKAN7M-BXTAVRT-2WZGOBR-QWMG6F7-3EHJ276-GUZJ2UW-ZVLRCQK"; }
    { name = "kvm-nixos-server"; id = "VMTWSEY-4TLKW4M-5KWF4NP-R44ZCUB-532E53K-WGO2YX3-GCRDHBV-3WGSHAI"; }
    { name = "nas"; id = "QZUWJON-LJGIKE7-EUZDUMN-EZGMKQ6-ET3QJ67-QTTJJOQ-NK3YB7A-52P76AE"; }
  ];

  deviceImports = builtins.map (device: import ./devices.nix {
    inherit config lib;
    name = device.name;
    id = device.id;
  }) syncthingDevices;

  syncthingFolders = [
    { name = "taskwarrior"; dirName = "taskwarrior_data"; }
    { name = "dev_resources"; dirName = "dev_resources"; }
    { name = "database"; dirName = "database"; }
    { name = "documents"; dirName = "documents"; }
    { name = "study"; dirName = "study"; }
    { name = "test"; dirName = "test"; }
  ];

  # Use builtins.map to import the folder module for each folder
  folderImports = builtins.map (folder: import ./folders.nix {
    inherit config lib;
    name = folder.name;
    dirName = folder.dirName;
  }) syncthingFolders;
in

{
  # Each machine switches in the devices and folders it needs.
  imports = deviceImports ++ folderImports ++ [];

  options.custom.services = {
    syncthing = lib.mkOption {
      type = lib.types.submodule {
        options = {
          httpPort = lib.mkOption {
            type = lib.types.int;
            default = 8384;
            description = "Port for Syncthing web GUI.";
          };

          dataDirectory = lib.mkOption {
            default = "/var/lib/syncthing/";
            type = lib.types.str;
            description = "Defines the Syncthing data directory";
          };

          syncDir = lib.mkOption {
            default = "${config.customGlobal.syncthing.syncDir}";
            type = lib.types.str;
            description = "Defines the Syncthing sync directory";
          };

          user = lib.mkOption {
            default = hostSpecific.primeUsername;
            type = lib.types.str;
            description = "Defines the Syncthing user";
          };

          simpleFileVersioningForBackUpMachinesOnly = lib.mkOption {
            type = lib.types.nullOr lib.types.attrs;
            default = null;
            description = "Syncthing simple versioning config, only enabled for homelab.";
          };
        };
      };
      default = {};
      description = "Syncthing related configuration";
    };
  };

  config = lib.mkIf config.services.syncthing.enable {

    systemd.tmpfiles.rules = [
      "d ${cfg.dataDirectory} 0750 ${cfg.user} ${config.customGlobal.dataGroup} -"
    ];

    sops.secrets."syncthing/cert.pem" = {
      owner = "${cfg.user}";
      mode = "0600";

    };

    sops.secrets."syncthing/key.pem" = {
      owner = "${cfg.user}";
      mode = "0600";
    };

    services.syncthing = {
      group = "${config.customGlobal.dataGroup}";
      user = "${cfg.user}";
      key = config.sops.secrets."syncthing/key.pem".path;
      cert = config.sops.secrets."syncthing/cert.pem".path;
      #overrideDevices = true; # Deletes devices that are not configured declaratively
      #overrideFolders = true; # Deletes folders that are not configured declaratively

      settings = {

        options = {
          urAccepted = -1; # explicitly disabled usage reporting.
          globalAnnounceEnabled = true;
          relaysEnabled = true;
          localAnnounceEnabled = true;
        };

        gui = {
          # user = config.sops.templates."syncuser".content;
          # password = config.sops.templates."syncpassword".content;
          theme = "black";
          insecureSkipHostCheck = true;
        };
      };
    };
  };
}
