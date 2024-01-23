{ config, pkgs, ... }:

{

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
    ../../modules/meta.nix
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
    ../../modules/home-manager/ssh.nix
    ../../modules/home-manager/polybar.nix
    ../../modules/home-manager/scripts/rofi-firefox-bookmarks.nix
    ../../modules/home-manager/scripts/helloworld-python.nix
    ../../modules/home-manager/scripts/tmux-sessionizer.nix
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
    ../../modules/home-manager/generic_linux_fonts.nix
  ];

  targets.genericLinux.enable = true;

  # Variables that will be set "system wide" in the context of the user.
  # E.g, by setting MANPAGER it will be available to bash, zsh, fish with
  # the alternative limiting the scope and setting this variables in a file
  # such as .bashrc using "export".
  home.sessionVariables = {
    EDITOR = config.userDefinedGlobalVariables.editor;
    SUDO_EDITOR = config.userDefinedGlobalVariables.editor;
    MANPAGER = config.userDefinedGlobalVariables.manPager;
  };

}
