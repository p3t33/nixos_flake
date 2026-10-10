{ config, lib, pkgs, ... }:
{

  config = lib.mkIf config.programs.yazi.enable {
    # Dependencies
    home.packages = with pkgs; [
      # Image preview in the terminal
      fzf
      ripgrep
      fd
      zoxide
    ];

    programs.yazi = {
      theme =
        let
          colors = {
            background = "#2e3440";
            foreground = "#d8dee9";
            accent = config.custom.shared.outlineColors.active or "#88c0d0";
            selection = "#3b4252";
            lighter_background = "#3b4252";
            dark_foreground = "#4c566a";
            green = "#a3be8c";
            red = "#bf616a";
            yellow = "#ebcb8b";
            magenta = "#b48ead";
            cyan = "#88c0d0";
          };
        in
        {
          mgr = {
            cwd = { fg = colors.accent; };
            find_keyword = { fg = colors.yellow; bold = true; italic = true; underline = true; };
            find_position = { fg = colors.magenta; bg = colors.background; bold = true; italic = true; };
            marker_copied = { fg = colors.green; bg = colors.green; };
            marker_cut = { fg = colors.red; bg = colors.red; };
            marker_marked = { fg = colors.cyan; bg = colors.cyan; };
            marker_selected = { fg = colors.accent; bg = colors.accent; };
            count_copied = { fg = colors.background; bg = colors.green; };
            count_cut = { fg = colors.background; bg = colors.red; };
            count_selected = { fg = colors.background; bg = colors.yellow; };
            border_style = { fg = colors.dark_foreground; };
          };
          tabs = {
            active = { fg = colors.background; bg = colors.accent; bold = true; };
            inactive = { fg = colors.dark_foreground; bg = colors.lighter_background; };
          };
          mode = {
            normal_main = { fg = colors.background; bg = colors.accent; bold = true; };
            normal_alt = { fg = colors.accent; bg = colors.lighter_background; };
            select_main = { fg = colors.background; bg = colors.green; bold = true; };
            select_alt = { fg = colors.green; bg = colors.lighter_background; };
            unset_main = { fg = colors.background; bg = colors.red; bold = true; };
            unset_alt = { fg = colors.red; bg = colors.lighter_background; };
          };
          indicator = {
            parent = { fg = colors.foreground; bg = colors.selection; reversed = false; };
            current = { fg = colors.accent; bg = colors.selection; reversed = false; };
            preview = { underline = true; };
          };
          status = {
            overall = { fg = colors.foreground; bg = colors.background; };
            perm_sep = { fg = colors.dark_foreground; };
            perm_type = { fg = colors.green; };
            perm_read = { fg = colors.yellow; };
            perm_write = { fg = colors.red; };
            perm_exec = { fg = colors.cyan; };
            progress_label = { bold = true; };
            progress_normal = { fg = colors.background; bg = colors.green; };
            progress_error = { fg = colors.background; bg = colors.red; };
          };
          which = {
            mask = { bg = colors.background; };
            cand = { fg = colors.cyan; };
            rest = { fg = colors.dark_foreground; };
            desc = { fg = colors.magenta; };
            separator_style = { fg = colors.dark_foreground; };
          };
          confirm = {
            border = { fg = colors.accent; };
            title = { fg = colors.accent; };
            btn_yes = { fg = colors.background; bg = colors.green; reversed = false; };
          };
          spot = {
            border = { fg = colors.accent; };
            title = { fg = colors.accent; };
            tbl_col = { fg = colors.accent; };
            tbl_cell = { fg = colors.yellow; bg = colors.selection; reversed = false; };
          };
          notify = {
            title_info = { fg = colors.green; };
            title_warn = { fg = colors.yellow; };
            title_error = { fg = colors.red; };
          };
          pick = {
            border = { fg = colors.accent; };
            active = { fg = colors.magenta; bold = true; };
          };
          input = {
            border = { fg = colors.accent; };
            selected = { fg = colors.background; bg = colors.accent; reversed = false; };
          };
          cmp = {
            border = { fg = colors.accent; };
            active = { fg = colors.background; bg = colors.accent; reversed = false; };
          };
          tasks = {
            border = { fg = colors.accent; };
            hovered = { fg = colors.accent; bold = true; };
          };
          help = {
            on = { fg = colors.cyan; };
            run = { fg = colors.magenta; };
            hovered = { fg = colors.accent; bg = colors.selection; reversed = false; bold = true; };
            footer = { fg = colors.background; bg = colors.foreground; };
          };
        };
      enableBashIntegration = true;
      enableZshIntegration = true;
      shellWrapperName = "y";

       keymap = {
        # Manager mode (“mgr” in the docs is “manager” in HM)
        manager.prepend_keymap = [
          # Bind Ctrl-n to open a draggable icon window for the hovered file
          # (use "$@" instead of "$1" if you want to drag multiple selected files)
          {
            on  = [ "<C-n>" ];
            run = ''shell --  dragon-drop -x -i -T "$1"'';
            desc = "Drag & drop (dragon-drop) for hovered file";
          }
        ];
      };
    };
 };
}
