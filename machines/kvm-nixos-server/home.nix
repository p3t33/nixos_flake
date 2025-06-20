{
  imports = [
    ./sops-home.nix
    ../../modules/meta.nix
    ../../modules/home-manager/basic.nix # enables home manger and sets the bare minimum.
    ../../modules/home-manager/custom-global-options/colors.nix
    ../../modules/home-manager/session-variables.nix
    ../../modules/home-manager/starship.nix
    ../../modules/home-manager/tmux.nix
    ../../modules/home-manager/fzf.nix
    ../../modules/home-manager/neovim/neovim.nix
    ../../modules/home-manager/git/git.nix
    ../../modules/home-manager/zoxide.nix
    ../../modules/home-manager/zsh.nix
    ../../modules/home-manager/bash.nix
    ../../modules/home-manager/taskwarrior.nix
    ../../modules/home-manager/scripts/tmux-sessionizer.nix
    ../../modules/home-manager/scripts/cheat-sh.nix
    ../../modules/home-manager/atuin.nix
    ../../modules/home-manager/git/lazygit.nix
    ../../modules/home-manager/gpg.nix
    ../../modules/home-manager/bat.nix
    ../../modules/home-manager/ssh/ssh-client.nix
    ../../modules/home-manager/yazi.nix
  ];

  customOptions = {
    enableModule.starship = true;
    enableModule.tmux = true;
    enableModule.fzf = true;
    enableModule.neovim = true;
    enableModule.git = true;
    enableModule.zoxide = true;
    enableModule.zsh = true;
    enableModule.bash = true;
    enableModule.taskwarrior = true;
    enableModule.atuin = true;
    enableModule.lazygit = true;
    enableModule.gpgAgent = true;
    enableModule.bat = true;
    enableModule.sshClient = true;
    enableModule.yazi = true;
  };

}
