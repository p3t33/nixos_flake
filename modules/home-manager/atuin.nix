{ config, lib, pkgs-unstable, ... }:

let
  themeName = "tokyo-night";
  palette = config.custom.shared.omarchyColors;
in
{

  config = lib.mkIf config.programs.atuin.enable {
    programs.atuin = {
      package = pkgs-unstable.atuin;
      enableZshIntegration = true;
      daemon.enable = true;

      flags = [ "--disable-up-arrow" ];

      settings = {
        local_timeout = 20;
        style = "compact";
        # for some reason this directive has no effect.
        show_help = false;
        enter_accept = true;
        search_mode = "daemon-fuzzy";
        ctrl_n_shortcuts = true; # instead of the alt default.

        theme = {
          name = themeName;
        };
      };

      themes = {
        "${themeName}" = {
          theme.name = themeName;
          colors = {
            AlertInfo = palette.green;
            AlertWarn = palette.yellow;
            AlertError = palette.red;
            Annotation = palette.accent;
            Base = palette.bright_foreground;
            Guidance = palette.foreground;
            Important = palette.accent;
            Title = palette.accent;
            Muted = palette.foreground;
            SyntaxCommand = palette.accent;
            SyntaxFlag = palette.cyan;
            SyntaxString = palette.green;
            SyntaxVariable = palette.magenta;
            SyntaxOperator = palette.bright_foreground;
            SyntaxComment = palette.dark_foreground;
          };
        };
      };
    };
  };
}
