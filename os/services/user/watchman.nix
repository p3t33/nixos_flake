{ config, pkgs, ... }:

{

  systemd.user.services.watchman = {
    description = "Watchman, a file watching service";

    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment} ; exec ${pkgs.watchman}/bin/watchman'";
      ExecStop = "${pkgs.watchman}/bin/watchman shutdown-server";
    };

    wantedBy = [ "default.target" ];
  };

}
