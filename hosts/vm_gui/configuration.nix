# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nicxos-help’).

{ config, pkgs, ... }:
let
  user = "kmedrish";
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./configuration-services.nix
      ../../os/fonts.nix
      ../../os/experimental-features.nix
      ../../os/garbage_collection.nix
      ../../os/system_version.nix
      ../../os/non_free_software.nix
      ../../os/locale.nix

    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  
  # docker
  virtualisation.docker.enable = true;

  networking.hostName = "kvm-nixos-gui"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = pkgs.zsh;
  users.users.${user} = {
    isNormalUser = true;
    initialPassword = "q";
    description = "${user}";
    extraGroups = [ "networkmanager" "wheel" "docker" "libvirtd"];
    packages = with pkgs; [];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    neovim
    wget
    emacs
    htop
    sshfs
    rofi-power-menu
    slack
    lsyncd
    plantuml
    graphviz
    jdk11
    git
    git-crypt
    gnupg
    pinentry_qt
    tcpdump
    wireshark
    rclone

    # required by neovim/vim for copy/past
    # to work with system clipboard on x11.
    xclip
    valgrind
    ansible
    speedtest-cli
    pulseaudio
    restic
    present
    slides
    polybar
    sumneko-lua-language-server
    ccls # LSP for C/CPP
    clang-tools # has clangd as part of it
    rnix-lsp
    vscode
    cherrytree
    alacritty
    gcc
    clang
    tmux
    zsh
    bat
    syncthing
    ncdu
    du-dust
    delta
    exa
    nmap
    fzf
    tree
    ripgrep
    openconnect
    cmake
    gnumake
    lua
    duf
    unzip
    whois
    plantuml
    inxi
    fd
    shellcheck
    arandr

    # Backend that is used by oh-my-zsh z plugin.
    # an alternative is to install autojump
    # to use the plugin exezute "z" from the shell.
    zsh-z

    universal-ctags

  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
}
