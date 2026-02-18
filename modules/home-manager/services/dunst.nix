{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.services.dunst.enable {
    services.dunst = {
      package = pkgs.dunst;
      settings = {
        global = {
          monitor = 0;
          follow = "mouse";
          border = 0;
          height = 400;
          width = 320;
          offset = "33x65";
          indicate_hidden = "yes";
          shrink = "no";
          separator_height = 0;
          padding = 32;
          horizontal_padding = 32;
          frame_width = 0;
          sort = "no";
          idle_threshold = 120;
          font = "Noto Sans";
          line_height = 4;
          markup = "full";
          format = "<b>%s</b>\n%b";
          alignment = "left";
          transparency = 10;
          show_age_threshold = 60;
          word_wrap = "yes";
          ignore_newline = "no";
          stack_duplicates = false;
          hide_duplicate_count = "yes";
          show_indicators = "no";
          icon_position = "left";
          icon_theme = "Adwaita-dark";
          sticky_history = "yes";
          history_length = 20;
          history = "ctrl+grave";
          browser = "firefox";
          always_run_script = true;
          title = "Dunst";
          class = "Dunst";
          max_icon_size = 64;

          mouse_left_click = "open_url, close_current";
          mouse_middle_click = "do_action, close_current";
          mouse_right_click = "close_all";
        };
      };
    };
  };
}
