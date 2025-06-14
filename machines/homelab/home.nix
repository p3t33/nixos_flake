{
  imports = [
    ./sops-home.nix
    ../../modules/home-manager/basic.nix # enables home manger and sets the bare minimum.
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
    ../../modules/home-manager/yazi.nix
  ];
}
