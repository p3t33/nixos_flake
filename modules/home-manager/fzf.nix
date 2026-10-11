{ config, lib, ... }:

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
        "bg+" = "#3b4252";
        "fg+" = "#b48ead";
        "hl+" = "#b48ead";
        selected-bg = "#3b4252";
        selected-fg = "#b48ead";
        selected-hl = "#b48ead";
      };
    };
  };
}
