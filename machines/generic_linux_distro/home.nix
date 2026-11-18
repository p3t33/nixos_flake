{ config, inputs, ... }:

{
  imports = [
    inputs.self.modules.generic.shared-options
    ./sops-home.nix
    inputs.self.modules.homeManager.home-core # enables home manger and sets the bare minimum.
    inputs.self.modules.homeManager.colors
    inputs.self.modules.homeManager.session-variables
    inputs.self.modules.homeManager.starship
    inputs.self.modules.homeManager.fzf
    inputs.self.modules.homeManager.neovim
    # inputs.self.modules.homeManager.git
    inputs.self.modules.homeManager.zoxide
    inputs.self.modules.homeManager.zsh
    inputs.self.modules.homeManager.bash
    inputs.self.modules.homeManager.tmux
    #inputs.self.modules.homeManager.syncthing
    #  inputs.self.modules.homeManager.rofi
    inputs.self.modules.homeManager.gpg
    inputs.self.modules.homeManager.ssh-client
    # Buku bookmarks are included with homeManager.rofi.
    #  inputs.self.modules.homeManager.helloworld-python
    inputs.self.modules.homeManager.tmux-sessionizer
    inputs.self.modules.homeManager.cheat-sh
    inputs.self.modules.homeManager.taskwarrior
    #  inputs.self.modules.homeManager.wallpaper
    inputs.self.modules.homeManager.atuin
    inputs.self.modules.homeManager.lazygit
    #  inputs.self.modules.homeManager.emacs
    #  inputs.self.modules.homeManager.xdg-mime-apps
    #  inputs.self.modules.homeManager.gtk
    #  inputs.self.modules.homeManager.qt
    #  inputs.self.modules.homeManager.dunst
    inputs.self.modules.homeManager.bat
    #  inputs.self.modules.homeManager.firefox
    #  inputs.self.modules.homeManager.generic-linux-fonts
    inputs.self.modules.homeManager.yazi
  ];

  programs.starship.enable = true;
  programs.fzf.enable = true;
  programs.neovim.enable = true;
  programs.zoxide.enable = true;
  programs.atuin.enable = true;
  programs.zsh.enable = true;
  programs.bash.enable = true;
  programs.tmux.enable = true;
  programs.ssh.enable = true;
  programs.taskwarrior.enable = true;
  programs.lazygit.enable = true;
  programs.bat.enable = true;
  programs.yazi.enable = true;
  programs.gpg.enable = true;

  custom.shared.sopsKeyPath = "${config.custom.shared.primeUserHomeDirectory}/.config/sops/age/keys.txt";
  targets.genericLinux.enable = true;
}
