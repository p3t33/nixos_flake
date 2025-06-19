{ config, lib, pkgs, hostSpecific, ... }:

let
  cfg = config.custom.services.moolticuted;
in
{
  options.custom.services.moolticuted.enable = lib.mkEnableOption "Enable the Moolticuted systemd service and udev rules";

  config = lib.mkIf cfg.enable {

    # Based on the systmed unit provided in the moolticute repository
    #
    # IMPORTANT: If this unit is used it is best to disable the option found
    # in the GUI called "Start Moolticute with the computer".

    systemd.services.moolticuted = {
      description = "Moolticute daemon";

      serviceConfig = {
        Type = "simple";
        # handles some race conditions, without this sleep, service fails with:
        # Failed to attach to shared segment:  "QSharedMemoryPrivate::initKey: unable to set key on lock"
        ExecStartPre = "${pkgs.coreutils}/bin/sleep 5";
        ExecStart = "${pkgs.moolticute}/bin/moolticuted";
        killMode = "process";
        Restart = "always";
        User = "${hostSpecific.primeUsername}";
        # --- Why 'Group=' is Not Used ---
        # No 'Group=' line is needed because the 'uaccess' tag already gives
        # the user specified above all the necessary permissions. Adding a
        # 'Group=' would be a redundant,

        CapabilityBoundingSet = "";
        RuntimeDirectory = "moolticuted";
        RuntimeDirectoryMode = 750;
        PrivateTmp = "yes";
        RemoveIPC = true;
        ProtectSystem = "strict";
        ProtectHome = "read-only";
        MemoryDenyWriteExecute = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = "AF_INET AF_INET6 AF_NETLINK AF_UNIX";
        RestrictNamespaces = true;
        RestrictRealtime = true;
      };

      wantedBy = [ config.systemd.targets.multi-user.name ];

    };

    services.udev.packages = [ pkgs.moolticute.udev ];
  };
}
