{
  config,
  lib,
  hostSpecific,
  ...
}:
{
  imports =
    [ ]
    ++ lib.optionals (hostSpecific.hostName == "work-pc") [
      ./devices/homelab.nix
      ./devices/home-desktop.nix
      ./folders/taskwarrior.nix
      ./folders/dev_resources.nix
    ]
    ++ lib.optionals (hostSpecific.hostName == "home-desktop") [
      ./devices/work_laptop.nix
      ./devices/homelab.nix
      ./folders/taskwarrior.nix
      ./folders/dev_resources.nix
      ./folders/database.nix
      ./folders/documents.nix
      ./folders/study.nix
    ]
    ++ lib.optionals (hostSpecific.hostName == "homelab") [
      ./devices/work_laptop.nix
      ./devices/home-desktop.nix
      ./folders/taskwarrior.nix
      ./folders/dev_resources.nix
      ./folders/database.nix
      ./folders/documents.nix
      ./folders/study.nix
    ];

  options.customOptions = {
    syncthing = lib.mkOption {
      type = lib.types.submodule {
        options = {
          httpPort = lib.mkOption {
            type = lib.types.int;
            default = 8384;
            description = "Port for Syncthing web GUI.";
          };

          devices = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            description = "avalible machines";
            default = {
              home-assistant = "home-assistant";
              work-pc = "work-pc";
              home-desktop = "home-desktop";
              homelab = "homelab";
            };
          };

          dataDirectory = lib.mkOption {
            default = "/var/lib/syncthing/";
            type = lib.types.str;
            description = "Defines the Syncthing data directory";
          };

          syncDir = lib.mkOption {
            default = "${config.customGlobalOptions.syncthing.syncDir}";
            type = lib.types.str;
            description = "Defines the Syncthing sync directory";
          };

          user = lib.mkOption {
            default = hostSpecific.primeUsername;
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
  };

  config = {

    systemd.tmpfiles.rules = [
      "d ${config.customOptions.syncthing.dataDirectory} 0750 ${config.customOptions.syncthing.user} ${config.customGlobalOptions.dataGroup} -"
    ];

    sops.secrets."syncthing/cert.pem" = {
      owner = "${config.customOptions.syncthing.user}";
      mode = "0600";

    };

    sops.secrets."syncthing/key.pem" = {
      owner = "${config.customOptions.syncthing.user}";
      mode = "0600";
    };

    services.syncthing = {
      enable = true;
      group = "${config.customGlobalOptions.dataGroup}";
      user = "${config.customOptions.syncthing.user}";
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
