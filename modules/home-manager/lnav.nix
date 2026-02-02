{ config, lib, pkgs, ... }:
let
  cfg = config.custom.lnav;
in
{
  options.custom.lnav = {
    enable = lib.mkEnableOption "lnav configuration";

    theme = lib.mkOption {
      type = lib.types.enum [
        "default"
        "dracula"
        "eldar"
        "grayscale"
        "modus-operandi"
        "monocai"
        "night-owl"
        "solarized-dark"
        "solarized-light"
      ];
      default = "monocai";
      description = "The lnav theme to use. monocai provides better contrast for fuzzy finder visibility.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Install lnav package
    home.packages = [ pkgs.lnav ];
    # Use xdg.configFile for XDG-compliant applications
    # This creates ~/.config/lnav/configs/default/config.json
    xdg.configFile."lnav/configs/default/config.json" = {
      text = builtins.toJSON {
        "$schema" = "https://lnav.org/schemas/config-v1.schema.json";
        ui = {
          theme = cfg.theme;
          clock-format = "%a %b %d %H:%M:%S";
          dim-text = false;
        };
        global = {
          default-session = "latest";
        };
      };
    };
  };
}
