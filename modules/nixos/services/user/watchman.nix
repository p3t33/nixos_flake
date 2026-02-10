{ config, lib, pkgs, ... }:

let
  cfg = config.custom.services.watchman;
in
{
  options.custom.services.watchman.enable = lib.mkEnableOption "Enable Watchman user-level service";

  config = lib.mkIf cfg.enable {
    systemd.user.services.watchman = {
      description = "Watchman, a file watching service";

      serviceConfig = {
        # SSH_AUTH_SOCK is required for executing commands that involve
        # ssh such as rsync. When using rsync in the context of systemd
        # there is no option to enter a password and so access to ssh-agent
        # is required. The path to the socket needs to be explicitly stated as
        # it is not included in the "setEnvironment".
        #
        # The string that is used as a value for SSH_AUTH_SOCK is NixOS
        # and will need to be changed for other OS, plus I am using
        # gpg-agent as a ssh-agnet.
        Environment = "SSH_AUTH_SOCK=/run/user/%U/gnupg/S.gpg-agent.ssh";
        Type = "forking";
        Restart = "on-failure";
        ExecStart = "${lib.getExe' pkgs.bash "bash"} -c 'source ${config.system.build.setEnvironment} ; ${lib.getExe pkgs.watchman}'";
        ExecStop = "${lib.getExe pkgs.watchman} shutdown-server";
      };

      wantedBy = [ "default.target" ];
    };
  };
}
