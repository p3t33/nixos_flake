{ config, pkgs, ... }:

{
    imports = [
        ../../modules/meta.nix
        ../../modules/home-manager/basic.nix # enables home manger and sets the bare minimum.
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
        ../../modules/home-manager/scripts/i3-monitor.nix
        ../../modules/home-manager/rofi.nix
        ../../modules/home-manager/gpg.nix
        ../../modules/home-manager/ssh.nix
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
}
