{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    arandr
    xorg.xrandr
  ];

  imports = [
    ../../modules/meta.nix
    ./sops-home.nix
    ../../modules/home-manager/basic.nix # enables home manger and sets the bare minimum.
    ../../modules/home-manager/custom-global-options/colors.nix
    ../../modules/home-manager/session-variables.nix
    ../../modules/home-manager/starship.nix
    ../../modules/home-manager/fzf.nix
    ../../modules/home-manager/neovim/neovim.nix
    # ../../modules/home-manager/git.nix
    ../../modules/home-manager/zoxide.nix
    ../../modules/home-manager/zsh.nix
    ../../modules/home-manager/bash.nix
    ../../modules/home-manager/tmux.nix
    ../../modules/home-manager/sxhkd.nix
    ../../modules/home-manager/syncthing.nix
    ../../modules/home-manager/i3status.nix
    ../../modules/home-manager/picom.nix
    ../../modules/home-manager/i3.nix
    ../../modules/home-manager/rofi.nix
    ../../modules/home-manager/gpg.nix
    ../../modules/home-manager/ssh/ssh-client.nix
    ../../modules/home-manager/polybar.nix
    ../../modules/home-manager/scripts/rofi-buku-bookmarks.nix
    ../../modules/home-manager/scripts/helloworld-python.nix
    ../../modules/home-manager/scripts/tmux-sessionizer.nix
    ../../modules/home-manager/scripts/cheat-sh.nix
    ../../modules/home-manager/redshift.nix
    ../../modules/home-manager/taskwarrior.nix
    ../../modules/home-manager/wallpaper.nix
    ../../modules/home-manager/atuin.nix
    ../../modules/home-manager/git/lazygit.nix
    ../../modules/home-manager/emacs/emacs.nix
    ../../modules/home-manager/xdg/mime_apps.nix
    ../../modules/home-manager/gtk.nix
    ../../modules/home-manager/qt.nix
    ../../modules/home-manager/services/dnust.nix
    ../../modules/home-manager/bat.nix
    ../../modules/home-manager/firefox.nix
    ../../modules/home-manager/generic_linux_fonts.nix
    ../../modules/home-manager/yazi.nix
  ];

  customOptions = {
    enableModule.starship = true;
    enableModule.fzf = true;
    enableModule.neovim = true;
    enableModule.zoxide = true;
    enableModule.zsh = true;
    enableModule.bash = true;
    enableModule.tmux = true;
    enableModule.rofi = true;
    enableModule.gpgAgent = true;
    enableModule.sshClient = true;
    enableModule.picom = true;
    enableModule.i3 = true;
    enableModule.polybar  = true;
    enableModule.redshift  = true;
    enableModule.taskwarrior = true;
    enableModule.wallpaper = true;
    enableModule.atuin = true;
    enableModule.lazygit = true;
    enableModule.mimeApps = true;
    enableModule.emacs = true;
    enableModule.gtk = true;
    enableModule.qt = true;
    enableModule.dunst = true;
    enableModule.bat = true;
    enableModule.firefox = true;
    enableModule.yazi = true;
  };


  customGlobalOptions.sopsKeyPath = "${config.customGlobalOptions.primeUserHomeDirectory }/.config/sops/age/keys.txt";
  targets.genericLinux.enable = true;
}
