{ config, lib, pkgs-unstable, ... }:

let
  themeName = "nord-desktop";
  accent = config.custom.shared.outlineColors.active;
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
            AlertInfo = "#a3be8c";
            AlertWarn = "#ebcb8b";
            AlertError = "#bf616a";
            Annotation = accent;
            Base = "#d8dee9";
            Guidance = "#81a1c1";
            Important = accent;
            Title = accent;
            Muted = "#a5abb6";
            SyntaxCommand = "#81a1c1";
            SyntaxFlag = accent;
            SyntaxString = "#a3be8c";
            SyntaxVariable = "#b48ead";
            SyntaxOperator = "#d8dee9";
            SyntaxComment = "#81a1c1";
          };
        };
      };
    };
  };
}
