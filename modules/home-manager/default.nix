{ config, lib, ... }:

let
  g = config.custom.profiles.homeManager;
in
{
  # ignored for now
  # ./syncthing.nix used by stand alone home-manager.
  # ./generic_linux_fonts.nix used by stand alone home-manger.
  imports = [
    ./session-variables.nix
    ./custom-global-options/colors.nix
    ../custom-global-options.nix

    ./core.nix
    ./starship.nix
    ./fzf.nix
    ./bat.nix
    ./neovim
    ./git/git.nix
    ./git/git_commit_signatures_with_gpg.nix
    ./git/lazygit.nix
    ./zoxide.nix
    ./zsh.nix
    ./bash.nix
    ./tmux.nix
    ./sxhkd.nix
    ./alacritty.nix
    ./picom.nix
    ./i3.nix
    ./i3status.nix
    ./i3/bars.nix
    ./polybar.nix
    ./rofi.nix
    ./gpg.nix
    ./ssh/ssh-client.nix
    ./ssh/ssh_public_key.nix
    ./redshift.nix
    ./taskwarrior.nix
    ./atuin.nix
    ./wallpaper.nix
    ./clipmenu.nix
    ./xdg/mime_apps.nix
    ./gtk.nix
    ./qt.nix
    ./firefox.nix
    ./zellij.nix
    ./yazi.nix
    ./ghostty.nix
    ./services/dnust.nix

    # scripts
    ./scripts/i3-monitor.nix
    ./scripts/rofi-buku-bookmarks.nix
    ./scripts/tmux-sessionizer.nix
    ./scripts/helloworld-python.nix
    ./scripts/cheat-sh.nix

    ./emacs
  ];

  options.custom.profiles.homeManager = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.enable = lib.mkEnableOption "Enable this Home Manager profile.";
    });
    default = {};
    description = "Enable system profiles like 'core', 'desktop', etc.";
  };

  config = lib.mkMerge [
     (lib.mkIf (g.desktop.enable or false) {
      programs.alacritty.enable = true;
      programs.ssh.enable = true;
      programs.rofi.enable = true;
      services.picom.enable = true;
      xsession.windowManager.i3.enable = true;
      services.polybar.enable = true;
      custom.scripts.i3Monitor.enable = true;
      services.redshift.enable = true;
      programs.taskwarrior.enable = true;
      custom.desktop.wallpaper.enable = true;
      xdg.mimeApps.enable = true;
      programs.emacs.enable = true;
      gtk.enable = true;
      qt.enable = true;
      services.dunst.enable = true;
      programs.firefox.enable = true;
      programs.ghostty.enable = true;
    })

    (lib.mkIf (g.core.enable or false) {
      programs.starship.enable = true;
      programs.tmux.enable = true;
      programs.zellij.enable = true;
      programs.fzf.enable = true;
      programs.neovim.enable = true;
      programs.git.enable = true;
      programs.zoxide.enable = true;
      programs.zsh.enable = true;
      programs.bash.enable = true;
      programs.bat.enable = true;
      programs.lazygit.enable = true;
      programs.atuin.enable = true;
      programs.yazi.enable = true;
      custom.scripts.cheatSh.enable = true;
    })
  ];
}

