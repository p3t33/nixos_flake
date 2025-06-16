{
  config,
  pkgs,
  ...
}:

let
  colors = rec {
    bg0 = "${config.customGlobalOptions.colors.background}E6";
    bg1 = "${config.customGlobalOptions.colors.background-alt}80";
    bg2 = "${config.customGlobalOptions.colors.primary}E6";
    fg0 = "#DEDEDE";
    fg1 = "${config.customGlobalOptions.colors.foreground}";
    fg2 = "${config.customGlobalOptions.colors.disabled}80";
  };
in
{
  programs.rofi = {
    enable = true;

    plugins = with pkgs; [
      rofi-calc
      rofi-emoji
      rofi-power-menu
    ];

    font = "${config.customGlobalOptions.font.sansSerif} 20";
    theme =
      let
        mkL = config.lib.formats.rasi.mkLiteral;
      in
      {
        "*" = {
          bg0 = mkL colors.bg0;
          bg1 = mkL colors.bg1;
          bg2 = mkL colors.bg2;
          fg0 = mkL colors.fg0;
          fg1 = mkL colors.fg1;
          fg2 = mkL colors.fg2;

          background-color = mkL "transparent";
          text-color = mkL "@fg0";

          margin = 0;
          padding = 0;
          spacing = 0;

        };

        window = {
          background-color = mkL "@bg0";
          location = mkL "center";
          width = 640;
          border-radius = 8;
        };

        inputbar = {
          padding = mkL "12px";
          spacing = mkL "12px";
          children = map mkL [
            "icon-search"
            "entry"
          ];
        };

        icon-search = {
          expand = false;
          filename = "search";
          size = mkL "28px";
          vertical-align = mkL "0.5";
        };

        entry = {
          placeholder = "Search";
          placeholder-color = mkL "@fg2";
          vertical-align = mkL "0.5";
        };

        message = {
          border = mkL "2px 0 0";
          border-color = mkL "@bg1";
          background-color = mkL "@bg1";
        };

        textbox = {
          padding = mkL "8px 24px";
        };

        listview = {
          lines = 10;
          columns = 1;
          fixed-height = false;
          border = mkL "1px 0 0";
          border-color = mkL "@bg1";
        };

        element = {
          padding = mkL "8px 16px";
          spacing = mkL "16px";
          background-color = mkL "transparent";
        };

        element-icon = {
          size = mkL "1em";
          vertical-align = mkL "0.5";
        };

        element-text = {
          text-color = mkL "inherit";
          vertical-align = mkL "0.5";
        };

        "element normal active" = {
          text-color = mkL "@bg2";
        };

        "element selected normal" = {
          background-color = mkL "@bg2";
          text-color = mkL "@fg1";
        };

        "element selected active" = {
          background-color = mkL "@bg2";
          text-color = mkL "@fg1";
        };
      };

    extraConfig = {
      show-icons = true;
      # I am using # as a delimiter because this is the recommendation on rofi man page for i3wm
      # This is the default list of models that will be available.
      modi = "drun#ssh#emoji#calc#power-menu:${pkgs.rofi-power-menu}/bin/rofi-power-menu#buku-bookmarks:rofi-buku-bookmakrs";
      terminal = "alacritty";
      sort = true;
      matching = "fuzzy";
      tokenize = true;
    };
  };

}
