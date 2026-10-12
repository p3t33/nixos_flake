{ config, lib, ... }:

{

  config = lib.mkIf config.programs.fzf.enable {
    programs.fzf = {
      enableZshIntegration = true;
      enableBashIntegration = true;

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
        "bg+" = "#363a42";
        "fg+" = "#ffffff:strip";
        "hl+" = "#88c0d0";
        pointer = "#88c0d0";
        marker = "#88c0d0";
        selected-bg = "#363a42";
        selected-fg = "#ffffff:strip";
        selected-hl = "#88c0d0";
      };
    };
  };
}
