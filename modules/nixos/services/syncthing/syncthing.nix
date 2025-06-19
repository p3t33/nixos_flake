{ config, lib, hostSpecific, ... }:

let
  cfg = config.customOptions.enableModule.syncthing ;
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

  options.customOptions = {
    enableModule.syncthing = lib.mkEnableOption "Enable Syncthing configuration module";

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
            default = "${config.customGlobalOptions.syncthing.syncDir}";
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

  config = lib.mkIf cfg {

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
