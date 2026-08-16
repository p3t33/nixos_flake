{ pkgs, config, lib, ... }:

let
  cfg = config.custom.services.sxhkd;
in
{
  options.custom.services.sxhkd.enable = lib.mkEnableOption "Enable sxhkd systemd user service";

  config = lib.mkIf cfg.enable {
    # I have used archlinux wiki as a reference for this unit.
    systemd.user.services.sxhkd = {
      description = "Simple X Hotkey Daemon";
      serviceConfig = {
        # the -c source... is used to get $PATH so I can execute software such as rofi.
        ExecStart = "${lib.getExe' pkgs.bash "bash"} -c 'source ${config.system.build.setEnvironment} ; exec ${lib.getExe pkgs.sxhkd}'";
        ExecReload = "${lib.getExe' pkgs.util-linux "kill"} -SIGUSR1 $MAINPID";
        Restart = "always";
        RestartSec = "2s";

      };
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
    };
  };
}

