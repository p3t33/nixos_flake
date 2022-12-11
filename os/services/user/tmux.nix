{ config, pkgs, ...}:

{

  systemd.user.services.tmux = {
    enable = true;
    description = "tmux server";

    # creates the [Service] section
    serviceConfig = {
      Type = "forking";
      #Environment = "PATH=/run/wrappers/bin:/home/${user}/.nix-profile/bin:/etc/profiles/per-user/${user}/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin";
      #ExecStart="${pkgs.tmux}/bin/tmux new-session -d";
      ExecStart="${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment} ; exec ${pkgs.tmux}/bin/tmux new-session -d'";
      ExecStop="${pkgs.tmux}/bin/tmux kill-server";
    };

    wantedBy = [ "default.target" ];

  };
}
