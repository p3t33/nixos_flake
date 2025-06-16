{
  config,
  lib,
  ...
}:
{
  options.customOptions = {
    wallpaperName = lib.mkOption {
      default = "watchtower.png";
      type = lib.types.str;
      description = "The name of the wallpaper file";
    };

    wallpaperOut = lib.mkOption {
      default = "wallpaper/${config.customOptions.wallpaperName}";
      type = lib.types.str;
      description = "Defines the path where the wallpaper will be located";
    };

    wallpaperIn = lib.mkOption {
      default = ../../wallpaper/${config.customOptions.wallpaperName};
      type = lib.types.path;
      description = "The relative path to the wallpaper file inside the repository";
    };
  };

  config = {
    xdg.configFile."${config.customOptions.wallpaperOut}".source = config.customOptions.wallpaperIn;
  };
}
