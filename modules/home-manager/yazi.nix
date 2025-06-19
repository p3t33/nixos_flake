{ config, lib, pkgs, ... }:

let
  cfg = config.customOptions.enableModule.yazi;
in
{
  options.customOptions.enableModule.yazi = lib.mkEnableOption "Enable Yazi file manager and its dependencies";

  config = lib.mkIf cfg {
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
 };
}
