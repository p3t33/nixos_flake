{ config, lib, ... }:
{
  config = lib.mkIf config.programs.zathura.enable {
    programs.zathura = {
      options = {
        selection-clipboard = "clipboard";
        selection-notification = false;
        font = "${config.custom.shared.font.mono} normal 16";
        scroll-page-aware = true;

        recolor = false; # start OFF, toggle with a key
        recolor-lightcolor = "#000000";
        recolor-darkcolor  = "#e5e5e5";
      };

      mappings = {
        "<F8>" = "recolor";
      };
    };
  };
}
