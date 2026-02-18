{
  lib,
  ...
}:
{
  options = {
    customGlobal = {
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
    };
  };
}
