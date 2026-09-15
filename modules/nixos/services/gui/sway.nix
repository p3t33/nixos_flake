{
  config,
  lib,
  pkgs,
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
  };
}
