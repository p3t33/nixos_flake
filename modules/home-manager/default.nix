{ config, lib, ... }:

let
  g = config.customOptions.enableHomeManagerProfile;
in
{

  # ignored for now
  # ./syncthing.nix used by stand alone home-manager.
  # ./generic_linux_fonts.nix used by stand alone home-manger.
  imports = [
    ./session-variables.nix # always enabled.
    ./custom-global-options/colors.nix
    ../custom-global-options.nix

    ./core.nix
    ./starship.nix
    ./fzf.nix
    ./bat.nix
    ./neovim # uses default.nix
    ./git/git.nix
    ./git/git_commit_signatures_with_gpg.nix
    ./git/lazygit.nix
    ./zoxide.nix
    ./zsh.nix
    ./bash.nix
    ./tmux.nix
    ./sxhkd.nix # will probably need globl variable as service configured in nixos
    ./alacritty.nix
    ./picom.nix
    ./i3.nix
    ./i3status.nix # not in use.
    ./i3/bars.nix # not in use.
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
    ./scripts/rofi-buku-bookmarks.nix # bind to rofi
    ./scripts/tmux-sessionizer.nix # bind to tmux.
    ./scripts/helloworld-python.nix
    ./scripts/cheat-sh.nix

    ./emacs  # uses default.nix
  ];

    options.customOptions.enableHomeManagerProfile = lib.mkOption {
      type = lib.types.attrsOf lib.types.bool;
      default = {};
      description = "Enable system profiles like 'desktop', 'server', 'securityKeys', etc.";
    };

     config.customOptions.enableModule = lib.mkMerge [

    # Core profile enables CLI, dev, encryption
    (lib.mkIf (g.core or false) {
      starship = true;
      tmux = true;
      zellij = true;
      fzf = true;
      neovim = true;
      git = true;
      zoxide = true;
      zsh = true;
      bash = true;
      cheatSh = true;
      atuin = true;
      lazygit = true;
      bat = true;
      yazi = true;
    })

    # Desktop profile enables GUI etc.
    (lib.mkIf (g.desktop or false) {
       alacritty = true;
       sshClient = true;
       rofi = true;
       picom = true;
       i3 = true;
       polybar  = true;
       i3Monitor = true;
       redshift  = true;
       taskwarrior = true;
       wallpaper = true;
       mimeApps = true;
       emacs = true;
       gtk = true;
       qt = true;
       dunst = true;
       firefox = true;
       ghostty = true;
    })
  ];

}

