{
  config,
  lib,
  machineName,
  ...
}:
{
  imports =
    [ ]
    ++ lib.optionals (machineName == "work-pc") [
      ./devices/homelab.nix
      ./devices/home-desktop.nix
      ./folders/taskwarrior.nix
      ./folders/dev_resources.nix
    ]
    ++ lib.optionals (machineName == "home-desktop") [
      ./devices/work_laptop.nix
      ./devices/homelab.nix
      ./folders/taskwarrior.nix
      ./folders/dev_resources.nix
      ./folders/database.nix
      ./folders/documents.nix
      ./folders/study.nix
    ]
    ++ lib.optionals (machineName == "homelab") [
      ./devices/work_laptop.nix
      ./devices/home-desktop.nix
      ./folders/taskwarrior.nix
      ./folders/dev_resources.nix
      ./folders/database.nix
      ./folders/documents.nix
      ./folders/study.nix
    ];

  systemd.tmpfiles.rules = [
    "d ${config.hostSpecification.syncthing.dataDirectory} 0750 ${config.hostSpecification.syncthing.user} ${config.userDefinedGlobalVariables.dataGroup} -"
  ];

  sops.secrets."syncthing/cert.pem" = {
    owner = "${config.hostSpecification.syncthing.user}";
    mode = "0600";

  };

  sops.secrets."syncthing/key.pem" = {
    owner = "${config.hostSpecification.syncthing.user}";
    mode = "0600";
  };

  services.syncthing = {
    enable = true;
    group = "${config.userDefinedGlobalVariables.dataGroup}";
    user = "${config.hostSpecification.syncthing.user}";
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
}
