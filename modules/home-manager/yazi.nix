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
    };
 };
}
