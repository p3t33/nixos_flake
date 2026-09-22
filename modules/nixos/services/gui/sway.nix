{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:

{
  config = lib.mkIf config.programs.sway.enable {
    programs.sway = {
      extraPackages = [ pkgs.swaybg ];
      extraSessionCommands = ''
        export XDG_SESSION_TYPE=wayland
      '';
    };

    xdg.portal.wlr.settings.screencast = {
      chooser_type = "simple";
      chooser_cmd = lib.getExe' pkgs-unstable.wlr-utils "wlr-chooser";
    };
  };
}
