{ config, pkgs, lib, ... }:

let
  cfg = config.custom.services.tmuxd;
in
{
  options.custom.services.tmuxd.enable = lib.mkEnableOption "Enable tmux systemd user service";

  config = lib.mkIf cfg.enable {
    systemd.user.services.tmux = {
      description = "tmux server";

      # creates the [Service] section
      # based on the emacs systemd service
      # does not source uses a login shell so does not load ~/.zshrc in case this is needed
      # just add -l(E.g bash -cl "...").
      serviceConfig = {
        Type = "forking";
        Restart = "on-failure";
        ExecStart = "${lib.getExe' pkgs.bash "bash"} -c 'source ${config.system.build.setEnvironment} ; ${lib.getExe pkgs.tmux} start-server'";
        ExecStop = "${lib.getExe pkgs.tmux} kill-server";
      };

      wantedBy = [ "default.target" ];
    };
  };
}
