{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    arandr
    xorg.xrandr
  ];

  imports = [
    ../../modules/custom-global-options.nix
    ./sops-home.nix
    ../../modules/home-manager/core.nix # enables home manger and sets the bare minimum.
    #../../modules/home-manager/custom-global-options/colors.nix
    ../../modules/home-manager/session-variables.nix
    ../../modules/home-manager/starship.nix
    ../../modules/home-manager/fzf.nix
    ../../modules/home-manager/neovim # uses default.nix
    # ../../modules/home-manager/git.nix
    ../../modules/home-manager/zoxide.nix
    ../../modules/home-manager/zsh.nix
    ../../modules/home-manager/bash.nix
    ../../modules/home-manager/tmux.nix
    #../../modules/home-manager/sxhkd.nix
    #../../modules/home-manager/syncthing.nix
  #  ../../modules/home-manager/i3status.nix
    ../../modules/home-manager/picom.nix
  #  ../../modules/home-manager/i3.nix
  #  ../../modules/home-manager/rofi.nix
    ../../modules/home-manager/gpg.nix
   ../../modules/home-manager/ssh/ssh-client.nix
  #  ../../modules/home-manager/polybar.nix
   # ../../modules/home-manager/scripts/rofi-buku-bookmarks.nix
  #  ../../modules/home-manager/scripts/helloworld-python.nix
   ../../modules/home-manager/scripts/tmux-sessionizer.nix
   ../../modules/home-manager/scripts/cheat-sh.nix
  #  ../../modules/home-manager/redshift.nix
   ../../modules/home-manager/taskwarrior.nix
  #  ../../modules/home-manager/wallpaper.nix
   ../../modules/home-manager/atuin.nix
   ../../modules/home-manager/git/lazygit.nix
  #  ../../modules/home-manager/emacs # uses default.nix
  #  ../../modules/home-manager/xdg/mime_apps.nix
  #  ../../modules/home-manager/gtk.nix
  #  ../../modules/home-manager/qt.nix
  #  ../../modules/home-manager/services/dnust.nix
   ../../modules/home-manager/bat.nix
  #  ../../modules/home-manager/firefox.nix
  #  ../../modules/home-manager/generic_linux_fonts.nix
   ../../modules/home-manager/yazi.nix
  ];

  customOptions = {
    enableModule = {
      starship = true;
      fzf = true;
      neovim = true;
      zoxide = true;
      atuin = true;
     zsh = true;
     bash = true;
     cheatSh = true;
     tmux = true;
     gpgAgent = true;
     sshClient = true;
  #    picom = true;
  #    redshift  = true;
     taskwarrior = true;
     lazygit = true;
  #    mimeApps = true;
  #    emacs = true;
  #    gtk = true;
  #    qt = true;
     bat = true;
     yazi = true;
    };
  };


  customGlobalOptions.sopsKeyPath = "${config.customGlobalOptions.primeUserHomeDirectory }/.config/sops/age/keys.txt";
  targets.genericLinux.enable = true;
}
