Personal [NixOs] configurations defined with [Home Manager] in a flake.
This repo is a work in progress, containing configuration files for my
various machines.

# Philosophy
- Multiple machines are defined with emphasis on shared code between them
in order to achieve consistency and to avoid code reuse where possible.
- Work flow is heavily skewed towards the use of the keyboard and the terminal.
Some keyboard bindings may look strange but they are effected by the fact
that I use a programmable keyboard(configurations can be located in
[my Adv360-Pro-ZMK repo]).
- Lacking functionality is extended by scripts(such as Firefox bookmarks for rofi).
- Effort has been made to define everything using nix, including the $HOME
dot files to the possible extent.

# Defaults
- **OS**: NixOS stable
- **Desktop**: xorg with i3 and polybar
- **Launcher**: Rofi
- **Shell**: zsh
- **Editor**: neovim
- **Terminal**: alacritty(with tmux).

# Repo design
- **flake.nix**: The entry point for hosts and home configurations.
- **home**: all the settings that are managed by home manager including user scripts.
- **hosts**: The machines that can be configured using this repository. With each
host having its high level .nix configuration that define host specific settings
and include code that can be shared with other hosts.
- **meta**: Global variables that are set per host.
- **os**: Is the core settings the can be found in /etc/nixos/configuration.nix and that
I refactored into responsibilities that can then be included by the various hosts
to achieve code reuse.
- **wallpaper**: self explanatory :)

# ToDo
- [ ] Generic way to mount hard drives that is not based on UUID but on instead.
- [ ] tmux save and restore sessions(using resurrect and continuum) not working.
  - [x] ~~Make tmux start as a systemd service after reboot in server mode.~~
  - [x] ~~Make tmux to restore saved sessions after reboot automatically.~~
  - [x] ~~Make tmux automatically save current session state at predefined intervals.~~
  - [ ] Fix the failure to restore applications(man, vim..).
- [ ] Move more variables into the meta.nix
- [ ] Eliminate the error that is caused by terminal command not found.


<!-- variables -->


[NixOS]: <https://nixos.org>
[Home Manager]: <https://github.com/nix-community/home-manager/>
[my Adv360-Pro-ZMK repo]: <https://github.com/p3t33/Adv360-Pro-ZMK/tree/V3.0/>
