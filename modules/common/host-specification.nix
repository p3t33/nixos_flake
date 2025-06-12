{ lib, config, ... }:

{
  options.hostSpecification = {
    systemStateVersion = lib.mkOption {
      type = lib.types.str;
      description = "NixOS system state version";
    };

    hostConfigurationName = lib.mkOption {
      type = lib.types.str;
      description = "Host config name";
    };

    motd = lib.mkOption {
      type = lib.types.nullOr lib.types.lines;
      description = "Message of the day";
    };

    sshPublicKey = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "SSH public key";
    };

    nvidiaHybridWithIntel = lib.mkOption {
      default = {
        nvidiaBusId = "";
        intelBusId = "";
      };
      type = lib.types.attrsOf lib.types.str;
      description = "Bus IDs for Nvidia Hybrid with Intel setup";
     };


    sopsKeyPath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "SOPS key file path (for non-NixOS)";
    };

    wallpaperName = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Wallpaper file name";
    };

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
            default = "${config.hostSpecification.primeUserHomeDirectory}/Sync";
            type = lib.types.str;
            description = "Defines the Syncthing sync directory";
          };

          user = lib.mkOption {
            default = config.hostSpecification.primeUsername;
            type = lib.types.str;
            description = "Defines the Syncthing user";
          };

          devicesToShareTaskWarriorFolderWith = lib.mkOption {
            default = [ ];
            type = lib.types.listOf lib.types.str;
            description = "List of devices to use for folder synchronization.";
          };

          devicesToShareDevResourcesFolderWith = lib.mkOption {
            default = [ ];
            type = lib.types.listOf lib.types.str;
            description = "List of devices to use for folder synchronization.";
          };

          devicesToShareDatabaseFolderWith = lib.mkOption {
            default = [ ];
            type = lib.types.listOf lib.types.str;
            description = "List of devices to use for folder synchronization.";
          };

          devicesToShareDocumentsFolderWith = lib.mkOption {
            default = [ ];
            type = lib.types.listOf lib.types.str;
            description = "List of devices to use for folder synchronization.";
          };

          devicesToShareStudyFolderWith = lib.mkOption {
            default = [ ];
            type = lib.types.listOf lib.types.str;
            description = "List of devices to use for folder synchronization.";
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

    primeUsername = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = "kmedrish";
      description = "Name of prime user on this host";
    };

    primeUserHomeDirectory = lib.mkOption {
        default = "/home/${config.hostSpecification.primeUsername}";
        type = lib.types.str;
        description = "Defines the primary user's home directory";
      };

  };
}
