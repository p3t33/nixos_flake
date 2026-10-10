{ config, lib, ... }:

let
  palette = config.custom.shared.omarchyColors;
in
{

  config = lib.mkIf config.programs.fzf.enable {
    programs.fzf = {
      enableZshIntegration = true;
      enableBashIntegration = true;

      # Note: Effect to the shell environment variables only took place after reboot.

      # rg can be used but fd is the better choice. It is purpose built for listing files,
      # which is what you want for fzf. ripgrep can list files too, but that
      # isn't its primary function. Its primary function is to search files.

      # CTRL-T - $FZF_CTRL_T_COMMAND
      fileWidgetCommand = ''fd --type file --type directory --hidden --exclude .git'';

      # ALT-C - $FZF_ALT_C_COMMAND
      changeDirWidgetCommand = "fd --type directory --hidden --exclude .git";

      # Default command that is executed for fzf - $FZF_DEFAULT_COMMAND
      defaultCommand = "fd --type file --hidden --exclude .git";

      colors = {
        "bg+" = palette.selection;
        "fg+" = palette.magenta;
        "hl+" = palette.magenta;
        selected-bg = palette.selection;
        selected-fg = palette.magenta;
        selected-hl = palette.magenta;
      };
    };
  };
}
