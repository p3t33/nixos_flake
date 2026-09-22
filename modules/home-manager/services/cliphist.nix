{ config, lib, ... }:
let
  cfg = config.custom.services.cliphist;
in
{
  options.custom.services.cliphist.enable = lib.mkEnableOption "Cliphist clipboard history";

  config = lib.mkIf cfg.enable {
    services.cliphist = {
      enable = true;
      allowImages = true;
      systemdTargets = [ "sway-session.target" ];
    };
  };
}
