{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "kmedrish";
  home.homeDirectory = "/home/kmedrish";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  imports = [
    ../modules/starship.nix
    ../modules/fzf.nix
    ../modules/neovim.nix
    ../modules/git.nix
    ../modules/zsh.nix
    ../modules/tmux.nix
    ../modules/ssh.nix
    ../modules/sxhkd.nix
    ../modules/syncthing.nix
    ../modules/i3status.nix
    ../modules/alacritty.nix
    ../modules/picom.nix
    ../modules/i3.nix.com
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
    SUDO_EDITOR = "nvim";
  };

}
