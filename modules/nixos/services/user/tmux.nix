{ config, pkgs, ...}:

{

  systemd.user.services.tmux = {
    enable = true;
    description = "tmux server";

    # creates the [Service] section
    # based on the emacs systemd service
    # does not source uses a login shell so does not load ~/.zshrc in case this is needed
    # just add -l(E.g bash -cl "...").
    serviceConfig = {
      Type = "forking";
      Restart = "always";
      ExecStart="${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment} ; exec ${pkgs.tmux}/bin/tmux start-server'";
      ExecStop="${pkgs.tmux}/bin/tmux kill-server";
    };

    wantedBy = [ "default.target" ];
  };
}
