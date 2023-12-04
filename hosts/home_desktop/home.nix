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
    ../../home/modules/starship.nix
    ../../home/modules/fzf.nix
    ../../home/modules/neovim/neovim.nix
    ../../home/modules/git.nix
    ../../home/modules/zoxide.nix
    ../../home/modules/zsh.nix
    ../../home/modules/bash.nix
    ../../home/modules/tmux.nix
    ../../home/modules/sxhkd.nix
    ../../home/modules/syncthing.nix
    ../../home/modules/i3status.nix
    ../../home/modules/alacritty.nix
    ../../home/modules/picom.nix
    ../../home/modules/i3.nix
    ../../home/modules/rofi.nix
    ../../home/modules/gpg.nix
    ../../home/modules/ssh/work/ssh.nix
    ../../home/modules/polybar.nix
    ../../home/scripts/rofi-firefox-bookmarks.nix
    ../../home/scripts/tmux-sessionizer.nix
    ../../home/scripts/helloworld-python.nix
    ../../home/scripts/cheat-sh.nix
    ../../home/modules/redshift.nix
    ../../home/modules/taskwarrior.nix
    ../../home/modules/wallpaper.nix
    ../../home/modules/nix-index.nix
    ../../home/modules/atuin.nix
    ../../home/modules/lazygit.nix
    ../../home/modules/clipmenu.nix
    ../../home/modules/emacs.nix
    ../../home/xdg/mime_apps.nix
    ../../home/modules/gtk.nix
    ../../home/services/dnust.nix
  ];

  home.sessionVariables = {
    EDITOR = config.userDefinedGlobalVariables.editor;
    SUDO_EDITOR = config.userDefinedGlobalVariables.editor;
    MANPAGER = config.userDefinedGlobalVariables.manPager;
  };
}
