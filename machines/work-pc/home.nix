{ config, ... }:
{
  imports = [
    ./sops-home.nix
    ../../modules/meta.nix
    ../../modules/home-manager/basic.nix # enables home manger and sets the bare minimum.
    ../../modules/home-manager/custom-global-options/colors.nix
    ../../modules/home-manager/session-variables.nix
    ../../modules/home-manager/starship.nix
    ../../modules/home-manager/fzf.nix
    ../../modules/home-manager/neovim/neovim.nix # to do
    ../../modules/home-manager/git/git.nix
    ../../modules/home-manager/git/git_commit_signatures_with_gpg.nix
    ../../modules/home-manager/zoxide.nix
    ../../modules/home-manager/zsh.nix
    ../../modules/home-manager/bash.nix
    ../../modules/home-manager/tmux.nix
    ../../modules/home-manager/sxhkd.nix # to do.
    ../../modules/home-manager/alacritty.nix
    ../../modules/home-manager/picom.nix
    ../../modules/home-manager/i3.nix
    ../../modules/home-manager/rofi.nix
    ../../modules/home-manager/gpg.nix
    ../../modules/home-manager/ssh/ssh-client.nix
    ../../modules/home-manager/ssh/ssh_public_key.nix
    ../../modules/home-manager/polybar.nix
    ../../modules/home-manager/scripts/i3-monitor.nix # to do
    ../../modules/home-manager/scripts/rofi-buku-bookmarks.nix #
    ../../modules/home-manager/scripts/helloworld-python.nix
    ../../modules/home-manager/scripts/tmux-sessionizer.nix
    ../../modules/home-manager/scripts/cheat-sh.nix
    ../../modules/home-manager/redshift.nix
    ../../modules/home-manager/taskwarrior.nix
    ../../modules/home-manager/wallpaper.nix
    ../../modules/home-manager/atuin.nix
    ../../modules/home-manager/git/lazygit.nix
    ../../modules/home-manager/emacs/emacs.nix # todo
    ../../modules/home-manager/xdg/mime_apps.nix
    ../../modules/home-manager/gtk.nix
    ../../modules/home-manager/qt.nix
    ../../modules/home-manager/services/dnust.nix
    ../../modules/home-manager/bat.nix
    ../../modules/home-manager/firefox.nix
    ../../modules/home-manager/zellij.nix
    ../../modules/home-manager/yazi.nix
    ../../modules/home-manager/ghostty.nix
  ];

  customOptions = {
    enableModule.starship = true;
    enableModule.fzf = true;
    enableModule.neovim = true;
    enableModule.git = true;
    enableModule.gitCommitSignaturesWithGpg = true;
    enableModule.zoxide = true;
    enableModule.zsh = true;
    enableModule.bash = true;
    enableModule.tmux = true;
    enableModule.alacritty = true;
    enableModule.rofi = true;
    enableModule.gpgAgent = true;
    enableModule.sshClient = true;
    enableModule.picom = true;
    enableModule.i3 = true;
    enableModule.enableSmartcardPublicKey  = true;
    enableModule.polybar  = true;
    enableModule.redshift  = true;
    enableModule.taskwarrior = true;
    enableModule.wallpaper = true;
    enableModule.atuin = true;
    enableModule.lazygit = true;
    enableModule.mimeApps = true;
    enableModule.gtk = true;
    enableModule.qt = true;
    enableModule.dunst = true;
    enableModule.bat = true;
    enableModule.firefox = true;
    enableModule.zellij = true;
    enableModule.yazi = true;
    enableModule.ghostty = true;


    wallpaperName = "watchtower.png";
    sshPublicKey = config.customGlobalOptions.sshPublicKeys.work-pc.key;
  };
}
