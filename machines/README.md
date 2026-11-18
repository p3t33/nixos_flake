# Machines

The flake automatically discovers NixOS hosts from `machines/<name>/configuration.nix`.
**The directory name becomes both the NixOS hostname and the key in `nixosConfigurations`.**
For example, `machines/home-desktop/configuration.nix` defines the host
`home-desktop` and is selected with `--flake .#home-desktop`.

Each host imports shared modules and defines its machine-specific settings,
such as hardware configuration. Directories without a `configuration.nix`,
such as `generic_linux_distro` for standalone Home Manager, are not discovered
as NixOS hosts.
