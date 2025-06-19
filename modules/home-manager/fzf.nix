{ config, lib, ... }:

let
  # catppuccinMocha = {
  #   bg        = "#1E1E2E";
  #   bg_plus   = "#313244";
  #   fg        = "#CDD6F4";
  #   fg_plus   = "#CDD6F4";
  #   hl        = "#F38BA8";
  #   hl_plus   = "#F38BA8";
  #   header    = "#F38BA8";
  #   prompt    = "#CBA6F7";
  #   info      = "#CBA6F7";
  #   spinner   = "#F5E0DC";
  #   pointer   = "#F5E0DC";
  #   marker    = "#B4BEFE";
  #   border    = "#313244";
  #   gutter    = "#1E1E2E";
  # };
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

      # colors = {
      #   "bg+"     = catppuccinMocha.bg_plus;
      #   bg        = catppuccinMocha.bg;
      #   spinner   = catppuccinMocha.spinner;
      #   hl        = catppuccinMocha.hl;
      #   fg        = catppuccinMocha.fg;
      #   header    = catppuccinMocha.header;
      #   gutter    = catppuccinMocha.gutter;
      #   info      = catppuccinMocha.info;
      #   pointer   = catppuccinMocha.pointer;
      #   marker    = catppuccinMocha.marker;
      #   "fg+"     = catppuccinMocha.fg_plus;
      #   prompt    = catppuccinMocha.prompt;
      #   "hl+"     = catppuccinMocha.hl_plus;
      #   border    = catppuccinMocha.border;
      # };
    };
  };
}
