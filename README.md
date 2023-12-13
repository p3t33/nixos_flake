Personal [NixOs] configurations defined with [Home Manager] in a flake.
This repo is a work in progress, containing configuration files for my
various machines.

# Design philosophy
- Multiple machines are defined with emphasis on shared code between them
in order to achieve consistency and to avoid code repeat where possible.
- Work flow is heavily skewed towards the use of the keyboard and the terminal.
Some keyboard bindings may look strange but they are effected by the fact
that I use a programmable keyboard(configurations can be located in
[my Adv360-Pro-ZMK repo]).
- Lacking functionality is extended by scripts(such as Firefox bookmarks for rofi).
- Effort has been made to define everything using nix, including the $HOME
dot files to the possible extent.

# Highlights
- Multiple hosts configurations, with home-manger as nixos module
  all reusing configuration.
- Stand alone home-manger configuration to be used on top of a
  generic Linux host such as Ubuntu.
- Secrets deployment using sops-nix.
- Extensively configured xorg and terminal environment.
- Configuration for KVM, VirtualBox, and docker.

## Daily driver software
- **Desktop**: xorg with i3 and polybar.
- **Launcher**: Rofi.
- **Shell**: zsh + starship with fzf and atuin integration.
- **Editor**: neovim.
- **Terminal**: alacritty(with tmux).
- **Second brain**: cherrytree.

## Some of the services I use
- **ssh-agnet(via gpg-agnet)**: For all my ssh needs.
- **emacs server**.
- **clipmenu**: a clipboard history via rofi.
- **redshift**: Adjusts the color temperature of your screen according to your surroundings.
- **dnust**: notification daemon.
- **sxhkd**: A simple X hotkey daemon.
- **syncthing**: sync files between all of my hosts.
- **tmux**: Used to start tmux on boot and with the resurrect and continuum
            plugins my entire terminal environment is ready for me on boot.
- **watchman**: Used to watch a directory and on any change in it to trigger actions.
  I used it to rsync files I changed locally to a remote automatically.
- **moolticuted**: a demon used to interact with mooltipass the hardware password manager.

# Repo structure
- **flake.nix**: The entry point for hosts and home configurations.
- **hosts**: The machines that can be configured using this repository. With each
host having its high level .nix configuration that define host specific settings
and include code that can be shared with other hosts.
- **modules**: Divided into modules to be used by home-manger(including user scripts)
to mange user configuration and into nixos modules which are mostly configurations
I found in /etc/nixos/configuration.nix and refactored into responsibilities that can
then be including by the various hosts to achieve code reuse.
- **meta**: Global variables that are set per host.
- **wallpaper**: self explanatory :)

# ToDo
- [ ] Generic way to mount hard drives that is not based on UUID but on labels instead.
- [x] ~~tmux save and restore sessions(using resurrect and continuum) not working.~~
  - [x] ~~Make tmux start as a systemd service after reboot in server mode.~~
  - [x] ~~Make tmux to restore saved sessions after reboot automatically.~~
  - [x] ~~Make tmux automatically save current session state at predefined intervals.~~
  - [x] ~~Fix the failure to restore applications(man, vim..).~~
  - [x] ~~Find a way for tmux to restore vim with vim session.~~
- [ ] Setup Emacs.
  - [x] ~~Create sane settings with some plugins to be used as a base.~~
  - [x] ~~Adjust Emacs settings to work with server-client mode. This is~~
        ~~a stretch goal as I installed Emacs for evaluation and right now~~
        ~~I am not sure I will be using it over neovim.~~
  - [ ] There is code duplication between the settings from Emacs and Emacs
        server in the forum of packages used. This needs to be eliminated.
  - [ ] I am not sure that the systemd for the Emacs server can communicate
        with ssh-agent. This is a very low priority.
- [x] ~~Move more variables into the meta.nix~~
- [x] ~~Eliminate the error that is caused by terminal command not found.~~
- [ ] Solve secret management
  - [x] ~~Integrate sops-nix into as nixos and home-manger module.~~
  - [ ] Hide sensitive data such as email and IP address from configuration files
    using sops.template.
  - [ ] Define user passwords as sops secrets.
  - [ ] Use sops for private keys storage.
  - [ ] Use sops for syncthing.

- [ ] Using standalone home-manger bring generic_linux_distro host which is
      intended for Ubuntu to be as close as possible to other hosts.


<!-- variables -->


[NixOS]: <https://nixos.org>
[Home Manager]: <https://github.com/nix-community/home-manager/>
[my Adv360-Pro-ZMK repo]: <https://github.com/p3t33/Adv360-Pro-ZMK/tree/V3.0/>
