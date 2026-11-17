{ ... }:
{
  flake.modules.homeManager.profile-home-manager = { config, lib, inputs, ... }:

let
  g = config.custom.profiles.homeManager;
in
{
  # ignored for now
  # ./syncthing.nix used by stand alone home-manager.
  # ./generic_linux_fonts.nix used by stand alone home-manger.
  imports = [
    inputs.self.modules.homeManager.session-variables
    inputs.self.modules.homeManager.colors
    inputs.self.modules.generic.shared-options

    inputs.self.modules.homeManager.home-core
    inputs.self.modules.homeManager.starship
    inputs.self.modules.homeManager.fzf
    inputs.self.modules.homeManager.antigravity
    inputs.self.modules.homeManager.claude
    inputs.self.modules.homeManager.codex
    inputs.self.modules.homeManager.bat
    inputs.self.modules.homeManager.neovim
    inputs.self.modules.homeManager.git
    inputs.self.modules.homeManager.delta
    inputs.self.modules.homeManager.git-signing
    inputs.self.modules.homeManager.lazygit
    inputs.self.modules.homeManager.zoxide
    inputs.self.modules.homeManager.direnv
    inputs.self.modules.homeManager.zsh
    inputs.self.modules.homeManager.bash
    inputs.self.modules.homeManager.tmux
    inputs.self.modules.homeManager.alacritty
    inputs.self.modules.homeManager.wezterm
    inputs.self.modules.homeManager.flameshot
    inputs.self.modules.homeManager.sway
    inputs.self.modules.homeManager.waybar
    inputs.self.modules.homeManager.rofi
    inputs.self.modules.homeManager.gpg
    inputs.self.modules.homeManager.ssh-client
    inputs.self.modules.homeManager.ssh-public-key
    inputs.self.modules.homeManager.wlsunset
    inputs.self.modules.homeManager.taskwarrior
    inputs.self.modules.homeManager.atuin
    inputs.self.modules.homeManager.wallpaper
    inputs.self.modules.homeManager.xdg-mime-apps
    inputs.self.modules.homeManager.xdg-user-dirs
    inputs.self.modules.homeManager.gtk
    inputs.self.modules.homeManager.qt
    inputs.self.modules.homeManager.moolticute
    inputs.self.modules.homeManager.glabels
    inputs.self.modules.homeManager.firefox
    inputs.self.modules.homeManager.whatsapp
    inputs.self.modules.homeManager.zellij
    inputs.self.modules.homeManager.yazi
    inputs.self.modules.homeManager.ghostty
    inputs.self.modules.homeManager.cliphist
    inputs.self.modules.homeManager.dunst
    inputs.self.modules.homeManager.mcp-gateway
    inputs.self.modules.homeManager.swayidle
    inputs.self.modules.homeManager.navi
    inputs.self.modules.homeManager.tealdeer
    inputs.self.modules.homeManager.pay-respects
    inputs.self.modules.homeManager.zathura
    inputs.self.modules.homeManager.lnav

    # scripts
    inputs.self.modules.homeManager.display-recovery
    inputs.self.modules.homeManager.tmux-sessionizer
    inputs.self.modules.homeManager.helloworld-python
    inputs.self.modules.homeManager.cheat-sh

    inputs.self.modules.homeManager.emacs
    inputs.self.modules.homeManager.mcp
    inputs.self.modules.homeManager.pi
  ];

  options.custom.profiles.homeManager = {
    core.enable = lib.mkEnableOption "core home-manager profile (shell, terminal, editor, git, CLI tools)";
    desktop = {
      common.enable = lib.mkEnableOption "common desktop home-manager profile";
      wayland.enable = lib.mkEnableOption "Sway desktop home-manager profile";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf g.desktop.common.enable {
      programs.alacritty.enable = true;
      programs.ssh.enable = true;
      programs.rofi.enable = true;
      programs.taskwarrior.enable = true;
      custom.desktop.wallpaper.enable = true;
      xdg.mimeApps.enable = true;
      programs.emacs.enable = true;
      gtk.enable = true;
      qt.enable = true;
      services.dunst.enable = true;
      programs.firefox.enable = true;
      programs.ghostty.enable = true;
      programs.zathura.enable = true;
      custom.programs.moolticute.enable = true;
      custom.programs.whatsapp.enable = true;
    })

    (lib.mkIf g.desktop.wayland.enable {
      wayland.windowManager.sway.enable = true;
      programs.waybar.enable = true;
      custom.services.cliphist.enable = true;
      services.udiskie.enable = true;
      services.flameshot.enable = true;
      services.wlsunset.enable = true;
      services.swayidle.enable = true;
    })

    (lib.mkIf g.core.enable {
      programs.starship.enable = true;
      programs.tmux.enable = true;
      programs.zellij.enable = true;
      programs.fzf.enable = true;
      programs.neovim.enable = true;
      programs.git.enable = true;
      programs.delta.enable = true;
      programs.zoxide.enable = true;
      programs.direnv.enable = true;
      programs.zsh.enable = true;
      programs.bash.enable = true;
      programs.bat.enable = true;
      programs.lazygit.enable = true;
      programs.atuin.enable = true;
      programs.yazi.enable = true;
      programs.navi.enable = true;
      programs.tealdeer.enable = true;
      programs.pay-respects.enable = true;
      programs.claude-code.enable = true;
      programs.codex.enable = true;
      programs.mcp.enable = true;
      custom.lnav.enable = true;
      custom.scripts.cheatSh.enable = true;
      custom.programs.pi.enable = true;
    })

  ];
};
}
