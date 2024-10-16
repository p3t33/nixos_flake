Personal [NixOS] configurations with [home-manager] as OS module(and as a stand alone
for generic GNU/Linux) in a flake. This repository is a work in progress, containing
configuration files for my various machines.

# Disclaimer
Since I'm employing [sops-nix] for secret management (such as user password)
within this repository, complete deployment is not feasible without the necessary
encryption key. Consequently, certain configurations will not function properly.

Use this repository as a blueprint for setting up your own system. The individual
configuration files (such as tmux, neovim, etc.) can serve as a reference.

# Design philosophy
- Multiple machines are defined with emphasis on shared code between them
in order to achieve consistency and to avoid code repeat where possible.
- Work flow is heavily skewed towards the use of the keyboard and the terminal.
Some keyboard bindings may look strange but they are effected by the fact
that I am using a programmable keyboard(configurations in
[my Adv360-Pro-ZMK repo]).
- Lacking functionality is extended by scripts(such as Firefox bookmarks for rofi).
- Effort has been made to define everything using nix, including the $HOME
dotfiles to the possible extent.

# Highlights
- Multiple host configurations using [home-manager] as a NixOS module, allowing for
  shared configurations across systems, with an option for standalone home-manager
  configurations to be used on generic Linux hosts, such as Ubuntu.
- Secrets deployment using [sops-nix].
- Extensively configured xorg and terminal environment.
- Configuration for KVM, VirtualBox, and docker.
- Integration of [disko] to partition hard drives during NixOS installing
  and to create /etc/fstab.

## Daily driver software
- **Desktop**: xorg with i3 and polybar.
- **Launcher**: Rofi.
- **Shell**: zsh + starship with fzf and atuin integration.
- **Editor**: neovim.
- **Terminal**: alacritty(with tmux).
- **Second brain**: emacs(org-roam).

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
- **moolticuted**: a daemon used to interact with mooltipass the hardware password manager.

# Repo structure
- **flake.nix**: The entry point for hosts and home configurations.
- **hosts**: The machines that can be configured using this repository. With each
host having its high level .nix configuration that define host specific settings
and include code that can be shared with other hosts.
- **modules**: Divided into modules to be used by home-manger(including user scripts)
to mange user configuration and into nixos modules which are mostly configurations
I found in /etc/nixos/configuration.nix and refactored into responsibilities that can
then be including by the various hosts to achieve code reuse.
- **wallpaper**: self explanatory :)

# ToDo
- [ ] Look into streamlining installation process of NixOS on a new host with [nixos-anywhere].
- [ ] Replace default way to mount hard drives with a generic one in the form of [disko].
  - [ ] Update all hosts to use disko.
- [ ] I am not sure that emacs systemd unit can communicate with ssh-agnet, this is a very low
      priority.
- [ ] Stream line the use of nix-index.
- [ ] Look into automating home-manger installation as a stand alone on
      generic Linux using Ansible.

<!-- variables -->


[NixOS]: <https://nixos.org>
[home-manager]: <https://github.com/nix-community/home-manager/>
[my Adv360-Pro-ZMK repo]: <https://github.com/p3t33/Adv360-Pro-ZMK/tree/V3.0/>
[disko]: <https://github.com/nix-community/disko>
[sops-nix]: <https://github.com/Mic92/sops-nix>
[nixos-anywhere]: <https://github.com/nix-community/nixos-anywhere>
