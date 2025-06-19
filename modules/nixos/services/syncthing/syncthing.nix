{ config, lib, hostSpecific, ... }:
let
  cfg = config.custom.services.syncthing;
in

{
  # Each machine switches in the devices and folders it needs.
  imports =[
      ./devices/work-pc.nix
      ./devices/home-desktop.nix
      ./devices/homelab.nix
      ./devices/kvm-nixos-server.nix
      ./folders/taskwarrior.nix
      ./folders/dev_resources.nix
      ./folders/database.nix
      ./folders/documents.nix
      ./folders/study.nix
    ];

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
