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
        Environment = [
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
          # Address rofi "xkbcommon: ERROR: ... string literal is not a valid UTF-8 string"
          "LC_CTYPE=en_US.UTF-8"
        ];

      };
      wantedBy = [ "default.target" ];
    };
  };
}

