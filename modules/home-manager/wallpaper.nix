{
  config,
  lib,
  pkgs,
  ...
}:

{
  xdg.configFile."${config.userDefinedGlobalVariables.wallpaperOut}".source =
    config.userDefinedGlobalVariables.wallpaperIn;
}
