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
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";


  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };


  # docker
  virtualisation.docker.enable = true;

  #KVM
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  services.udev.packages = [ pkgs.moolticute.udev ];

  networking.hostName = "HP-Zbook"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;
  
  networking.interfaces.enp0s20f0u6u3u1.useDHCP = false;
  networking.interfaces.enp0s20f0u6u3u1.ipv4.addresses = [ {
    address = "192.168.99.1";
    prefixLength = 24;
  } ];


  # Set your time zone.
  time.timeZone = "Asia/Jerusalem";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_IL";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "he_IL.UTF-8";
    LC_IDENTIFICATION = "he_IL.UTF-8";
    LC_MEASUREMENT = "he_IL.UTF-8";
    LC_MONETARY = "he_IL.UTF-8";
    LC_NAME = "he_IL.UTF-8";
    LC_NUMERIC = "he_IL.UTF-8";
    LC_PAPER = "he_IL.UTF-8";
    LC_TELEPHONE = "he_IL.UTF-8";
    LC_TIME = "he_IL.UTF-8";
  };

  # Enable sound
  sound.enable = true;
  #need to be flase or it will conflict with PipeWire..
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;

  # If you want to use JACK applications, uncomment t    his
  #jack.enable = true;

  # use the example session manager (no others are pa    ckaged yet so this is enabled by default,
  # no need to redefine it in your config for now)
  #media-session.enable = true;
  };


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.defaultUserShell = pkgs.zsh;
  users.users.${user} = {
    isNormalUser = true;
    initialPassword = "changeme";
    description = "${user}";
    extraGroups = [ "networkmanager" "wheel" "docker" "libvirtd" ];
    packages = with pkgs; [];
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  environment.variables.EDITOR = "nvim";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    neovim
    wget
    firefox
    emacs
    htop
    gimp
    sshfs
    present
    slides
    rofi-power-menu
    slack
    lsyncd
    plantuml
    graphviz
    jdk11
    git
    git-crypt
    gnupg
    # used by gpg-agent as a gui popup
    pinentry_qt
    picocom
    tcpdump
    wireshark
    moolticute
    polybar
    rclone

    # required by neovim/vim for copy/past
    # to work with system clipboard on x11.
    xclip
    valgrind
    ansible
    speedtest-cli
    pulseaudio
    restic
    gcc
    sumneko-lua-language-server
    ccls # LSP for C/CPP
    clang-tools # has clangd as part of it
    clang
    rnix-lsp
    vscode
    cherrytree
    tmux
    gparted
    dmenu
    zsh
    bat
    syncthing
    vlc
    ncdu
    du-dust
    delta
    exa
    nmap
    alacritty
    moolticute
    fzf
    tree
    ripgrep
    cryptomator
    veracrypt
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
    xorg.xrandr
    arandr
    google-chrome
    nitrogen # A wallpaper browser and setter for X11
    picom # A fork of XCompMgr, a sample compositing manager for X servers
    gnome.nautilus
    flameshot
    sxhkd

    # Backend that is used by oh-my-zsh z plugin.
    # an alternative is to install autojump
    # to use the plugin exezute "z" from the shell.
    zsh-z

    # U2F libraries - this needs to be tested because not all
    # packages that are installed on Ubuntu 22.04 were installed
    # and found here.
    libfido2 #webauto
    pam_u2f #linux system

    # Virt-manager
    virt-manager
    qemu_kvm
    qemu
    libvirt

    # infrastructure as code
    packer
    vagrant
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
