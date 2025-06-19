{ config, lib, ... }:

let
  catppuccinTheme = "catppuccin-mocha-blue";
  cfg = config.customOptions.enableModule.atuin;
in
{
  options.customOptions.enableModule.atuin = lib.mkEnableOption "Enable Atuin shell history replacement";

  config = lib.mkIf cfg {
    programs.atuin = {
      enable = true;
      enableZshIntegration = true;

      flags = [ "--disable-up-arrow" ];

      settings = {

        style = "compact";
        # for some reason this directive has no effect.
        show_help = false;

        theme = {
          name = catppuccinTheme;
        };
      };

      themes = {
        "${catppuccinTheme}" = {
          theme.name = catppuccinTheme;
          colors = {
            AlertInfo = "#a6e3a1";
            AlertWarn = "#fab387";
            AlertError = "#f38ba8";
            Annotation = "#89b4fa";
            Base = "#cdd6f4";
            Guidance = "#9399b2";
            Important = "#f38ba8";
            Title = "#89b4fa";
          };
        };
      };
    };
  };
}
