{ pkgs,  ... }:
{

  systemd.user.services.sxhkd = {
    enable = true;
    description = "Simple X Hotkey Daemon";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];

    # creates the [Service] section
    serviceConfig = {
      ExecStart="${pkgs.sxhkd}/bin/sxhkd";
      #ExecStop="${pkgs.kill}/bin/kill -SIGUSR1 $MAINPID";
    };

  };
}
