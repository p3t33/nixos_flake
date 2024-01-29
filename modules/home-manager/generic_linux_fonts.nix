# This file is only to be used for on a stand alone
# installation of home-manger.

{ pkgs, config, ... }:
{
    home.packages = config.userDefinedGlobalVariables.fontPackages;
    # Required to autoload fonts from packages for the system to use.
    fonts.fontconfig.enable = true;
}
