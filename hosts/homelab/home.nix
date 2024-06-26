{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = config.userDefinedGlobalVariables.primeUsername;
  home.homeDirectory = config.userDefinedGlobalVariables.primeUserHomeDirectory;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = config.userDefinedGlobalVariables.homeManagerStateVersion;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  imports = [
    ../../modules/meta.nix
    ../../modules/home-manager/starship.nix
    ../../modules/home-manager/tmux.nix
    ../../modules/home-manager/fzf.nix
    ../../modules/home-manager/neovim/neovim.nix
    ../../modules/home-manager/git.nix
    ../../modules/home-manager/zoxide.nix
    ../../modules/home-manager/zsh.nix
    ../../modules/home-manager/bash.nix
    ../../modules/home-manager/taskwarrior.nix
    ../../modules/home-manager/scripts/tmux-sessionizer.nix
    ../../modules/home-manager/scripts/cheat-sh.nix
    ../../modules/home-manager/nix-index.nix
    ../../modules/home-manager/atuin.nix
    ../../modules/home-manager/lazygit.nix
    ../../modules/home-manager/gpg.nix
    ../../modules/home-manager/bat.nix
    ../../modules/home-manager/ssh.nix
  ];

  home.sessionVariables = {
      EDITOR = config.userDefinedGlobalVariables.editor;
      SUDO_EDITOR = config.userDefinedGlobalVariables.editor;
      MANPAGER = config.userDefinedGlobalVariables.manPager;
  };
}
