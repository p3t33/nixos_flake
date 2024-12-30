{ pkgs, inputs, ... }:
{
  home.packages = with pkgs; [
      inputs.ghostty.packages.${system}.default
  ];

  home.file = {
    ".config/ghostty/config".text = ''
      font-family = JetBrainsMono Nerd Font
      font-size = 16
      theme = nord
      #gtk-tabs-location = hidden
      #mouse-hide-while-typing = true
      background-opacity = 0.96
      copy-on-select = clipboard
      #background = #111111
      #foreground = #08D9D6
      window-decoration = true
      gtk-titlebar = false
      gtk-adwaita = false
      #cursor-invert-fg-bg = true

      confirm-close-surface = false


     # selectiong of text
     #selection-background = 1d3c3b
     #selection-foreground = #08D9D6
    '';
  };

}
