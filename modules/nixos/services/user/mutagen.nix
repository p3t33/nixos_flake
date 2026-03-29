{ config, lib, pkgs, ... }:

let
  cfg = config.custom.services.mutagen;
in
{
  options.custom.services.mutagen.enable = lib.mkEnableOption "Enable Mutagen user-level service";

  config = lib.mkIf cfg.enable {
    # The systemd service references mutagen via its Nix store path,
    # which doesn't make it available in the user's shell PATH.
    environment.systemPackages = [ pkgs.mutagen ];

    systemd.user.services.mutagen = {
      description = "Mutagen file synchronization daemon";

      serviceConfig = {
        Type = "forking";
        Restart = "on-failure";
        ExecStart = "${lib.getExe' pkgs.bash "bash"} -c 'source ${config.system.build.setEnvironment} ; ${lib.getExe' pkgs.mutagen "mutagen"} daemon start'";
        ExecStop = "${lib.getExe' pkgs.mutagen "mutagen"} daemon stop";
      };

      wantedBy = [ "default.target" ];
    };
  };
}
