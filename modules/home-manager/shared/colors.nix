{
  lib,
  ...
}:
{
  options.custom.shared = {
    colors = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = {
          background = "#312f2f";
          background-alt = "#3b4354";
          foreground = "#F1FAEE";
          primary = "#08D9D6";
          secondary = "#047672";
          alert = "#ff2e63";
          disabled = "#707880";
          bg = "#2f343f";
          inactive-bg = "#2f343f";
          text = "#f3f4f5";
          inactive-text = "#676E70";
          urgent-bg = "#E53935";
        };
      description = "Defines the color palette for the user interface";
    };

    outlineColors = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {
        active = "#88c0d0";
        inactive = "#282c34";
      };
      description = "Window and popup outline colors";
    };

    surfaceBackground = lib.mkOption {
      type = lib.types.str;
      default = "#282c34";
      description = "Background color for desktop surfaces";
    };

    omarchyColors = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {
        accent = "#7aa2f7";
        selection = "#292e42";
        muted = "#414868";
        background = "#1a1b26";
        dark_background = "#13141c";
        darker_background = "#0e0e14";
        lighter_background = "#24283b";
        foreground = "#a9b1d6";
        dark_foreground = "#565f89";
        light_foreground = "#b4bee6";
        bright_foreground = "#c0caf5";
        red = "#f7768e";
        yellow = "#e0af68";
        orange = "#eb927b";
        green = "#9ece6a";
        cyan = "#449dab";
        blue = "#7aa2f7";
        magenta = "#ad8ee6";
        brown = "#75493d";
        bright_red = "#ff7a93";
        bright_yellow = "#ff9e64";
        bright_green = "#b9f27c";
        bright_cyan = "#0db9d7";
        bright_blue = "#7da6ff";
        bright_magenta = "#bb9af7";
        inactive_border = "#595959aa";
      };
      description = "Omarchy Tokyo Night palette";
    };
  };
}
