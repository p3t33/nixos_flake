{ config, pkgs, ... }:

{

  # IMPORTANT: hostname must be defined!
  # All of the global variables are defined based on the value set for it. Many
  # files use them and by not setting the hostname they will be using thier
  # default values which may cause all kind of issues.
  userDefinedGlobalVariables = {
      hostname = "kvm-nixos-server";
  };

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = config.userDefinedGlobalVariables.username;
  home.homeDirectory = config.userDefinedGlobalVariables.homeDirectory;

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
    ../../meta/meta.nix
    ../../home/modules/starship.nix
    ../../home/modules/fzf.nix
    ../../home/modules/neovim.nix
    ../../home/modules/git.nix
    ../../home/modules/zsh.nix
    ../../home/modules/taskwarrior.nix
    ../../home/scripts/tmux-sessionizer.nix
    ../../home/scripts/cheat-sh.nix
    ../../home/modules/nix-index.nix

  ];

  home.sessionVariables = {
      EDITOR = config.userDefinedGlobalVariables.editor;
      SUDO_EDITOR = config.userDefinedGlobalVariables.editor;
  };


}
