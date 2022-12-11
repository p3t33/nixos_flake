{ ... }:

{

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;

     # CTRL-T
    fileWidgetCommand = "fd --hidden";

    # ALT-C
    changeDirWidgetCommand = "fd --hidden --type d";

    # Default command that is executed for fzf
    defaultCommand = "fd --hidden --type f";
    };

}
