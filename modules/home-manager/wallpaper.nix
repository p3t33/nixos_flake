{ config, lib, ... }:

let
  cfg = config.custom.desktop.wallpaper;
  pathIn = ../../wallpaper/${cfg.name};
in
{
  options.custom.desktop.wallpaper = {
    enable = lib.mkEnableOption "Enable wallpaper setup";

    name = lib.mkOption {
      type = lib.types.str;
      default = "watchtower.png";
      description = "The name of the wallpaper file";
    };

    pathOut = lib.mkOption {
      type = lib.types.str;
      default = ""; # will be set below in config using mkDefault
      description = "The path where the wallpaper will be located inside the XDG config";
    };

    pathIn = lib.mkOption {
      type = lib.types.path;
      default = ../../wallpaper/watchtower.png; # will be overridden below based on wallpaperName
      description = "The source path to the wallpaper inside the repository";
    };

  };

  config = lib.mkIf cfg.enable {
    custom.desktop.wallpaper.pathOut = lib.mkDefault "wallpaper/${cfg.name}";
    custom.desktop.wallpaper.pathIn = lib.mkDefault ../../wallpaper/${cfg.name};

    xdg.configFile."${cfg.pathOut}".source = pathIn; # Use the internal wallpaperPathIn
  };
}
