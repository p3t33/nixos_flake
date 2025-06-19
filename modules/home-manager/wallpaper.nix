{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.wallpaper;
in
{
  options.customOptions = {
    enableModule.wallpaper = lib.mkEnableOption "Enable wallpaper setup";

    wallpaperName = lib.mkOption {
      type = lib.types.str;
      default = "watchtower.png";
      description = "The name of the wallpaper file";
    };

    wallpaperOut = lib.mkOption {
      type = lib.types.str;
      default = ""; # will be set below in config using mkDefault
      description = "The path where the wallpaper will be located inside the XDG config";
    };

    wallpaperIn = lib.mkOption {
      type = lib.types.path;
      default = ../../wallpaper/watchtower.png; # will be overridden below based on wallpaperName
      description = "The source path to the wallpaper inside the repository";
    };
  };

  config = lib.mkIf cfg {
    customOptions.wallpaperOut = lib.mkDefault "wallpaper/${config.customOptions.wallpaperName}";
    customOptions.wallpaperIn  = lib.mkDefault ../../wallpaper/${config.customOptions.wallpaperName};

    xdg.configFile."${config.customOptions.wallpaperOut}".source = config.customOptions.wallpaperIn;
  };
}
