{
  userDefinedGlobalVariables,
  ...
}:

{
  xdg.configFile."${userDefinedGlobalVariables.wallpaperOut}".source =
    userDefinedGlobalVariables.wallpaperIn;
}
