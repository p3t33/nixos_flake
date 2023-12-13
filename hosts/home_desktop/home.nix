{ config, pkgs, ... }:

{

  # IMPORTANT: hostname must be defined!
  # All of the global variables are defined based on the value set for it. Many
  # files use them and by not setting the hostname they will be using thier
  # default values which may cause all kind of issues.
  userDefinedGlobalVariables = {
      hostname = "home-desktop";
  };

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = config.userDefinedGlobalVariables.username;
  home.homeDirectory = config.userDefinedGlobalVariables.homeDirectory;
  #home.homeDirectory = "/home/kmedrish";

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
    ../../modules/home-manager/starship.nix
    ../../modules/home-manager/fzf.nix
    ../../modules/home-manager/neovim/neovim.nix
    ../../modules/home-manager/git.nix
    ../../modules/home-manager/zoxide.nix
    ../../modules/home-manager/zsh.nix
    ../../modules/home-manager/bash.nix
    ../../modules/home-manager/tmux.nix
    ../../modules/home-manager/sxhkd.nix
    ../../modules/home-manager/syncthing.nix
    ../../modules/home-manager/i3status.nix
    ../../modules/home-manager/alacritty.nix
    ../../modules/home-manager/picom.nix
    ../../modules/home-manager/i3.nix
    ../../modules/home-manager/rofi.nix
    ../../modules/home-manager/gpg.nix
    ../../modules/home-manager/ssh/work/ssh.nix
    ../../modules/home-manager/polybar.nix
    ../../modules/home-manager/scripts/rofi-firefox-bookmarks.nix
    ../../modules/home-manager/scripts/tmux-sessionizer.nix
    ../../modules/home-manager/scripts/helloworld-python.nix
    ../../modules/home-manager/scripts/cheat-sh.nix
    ../../modules/home-manager/redshift.nix
    ../../modules/home-manager/taskwarrior.nix
    ../../modules/home-manager/wallpaper.nix
    ../../modules/home-manager/nix-index.nix
    ../../modules/home-manager/atuin.nix
    ../../modules/home-manager/lazygit.nix
    ../../modules/home-manager/clipmenu.nix
    ../../modules/home-manager/emacs.nix
    ../../modules/home-manager/xdg/mime_apps.nix
    ../../modules/home-manager/gtk.nix
    ../../modules/home-manager/services/dnust.nix
    ../../modules/home-manager/firefox.nix
  ];

  home.sessionVariables = {
    EDITOR = config.userDefinedGlobalVariables.editor;
    SUDO_EDITOR = config.userDefinedGlobalVariables.editor;
    MANPAGER = config.userDefinedGlobalVariables.manPager;
  };
}
