{  pkgs, config, ... }:
{
  # I have used archlinux wiki as a reference for this unit.
  systemd.user.services.sxhkd = {
    enable = true;
    description = "Simple X Hotkey Daemon";
    serviceConfig = {
      # the -c source... is used to get $PATH so I can execute software such as rofi.
      ExecStart = "${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment} ; exec ${pkgs.sxhkd}/bin/sxhkd'";
      ExecReload = "${pkgs.coreutils}/bin/kill -SIGUSR1 $MAINPID";
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
}

