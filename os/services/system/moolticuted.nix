{ pkgs, ...}:
{

  systemd.services.moolticuted = {
    enable = true;
    unitConfig = {
      description = "Moolticute daemon";
    };

    serviceConfig = {
      Type = "simple";
      ExecStart="${pkgs.moolticute}/bin/moolticuted";
      killMode = "process";
      Restart = "always";
      User= "kmedrish";

      CapabilityBoundingSet = "";
      RuntimeDirectory = "moolticuted";
      RuntimeDirectoryMode = 750;
      PrivateTmp= "yes";
      RemoveIPC= true;
      ProtectSystem= "strict";
      ProtectHome= "read-only";
      MemoryDenyWriteExecute = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = "AF_INET AF_INET6 AF_NETLINK AF_UNIX";
      RestrictNamespaces = true;
      RestrictRealtime = true;
    };

    #installConfig = {
    #  wantedBy = [ "multi-user.target" ];
    #};
    wantedBy = [ "multi-user.target" ];
  };

}
