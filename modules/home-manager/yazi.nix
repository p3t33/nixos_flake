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
      enableBashIntegration = true;
      enableZshIntegration = true;

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
