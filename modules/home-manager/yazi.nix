{ pkgs, ... }:
{
  # Dependencies
  home.packages = with pkgs; [
    # Image preview in the terminal
    fzf
    ripgrep
    fd
    zoxide
  ];

  programs.yazi = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

}
