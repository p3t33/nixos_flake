{ config, lib, pkgs, pkgs-unstable, ... }:

let
  cfg = config.custom.apps.cliUtilities;
in
{
  options.custom.apps.cliUtilities.enable = lib.mkEnableOption "Enable general tools, utilities, and media/networking programs";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # presentation
      present
      slides
      haskellPackages.patat
      w3m

      # media
      mpv
      ffmpeg_7-full # video editing.

      # web
      wget
      speedtest-go
      pkg-config # helper tool for more information during the update of nix.

      # process monitor
      htop-vim
      neovim

      # networking
      tcpdump
      nmap
      whois
      openconnect
      gp-saml-gui

      # search
      ripgrep
      fd
      fzf

      # system information
      inxi
      ncdu
      dust
      duf
      gdu

      # disk utils
      gptfdisk
      ntfs3g
      geteltorito # extract boot image from CD/DVD/ISO. I used it to uprade bios.

      # general
      sshfs
      sshpass
      lsyncd
      rclone
      watchman
      restic
      bat
      unzip
      zip
      tmux
      smug # Session manager for tmux.
      moreutils # I need the sponge appliction to be used as part of tmux configuration.
      exfat # required by Ventoy installer.

      lshw

      eza
      tree

      zsh

      # A cross shell smarter cd command, inspired by z and autojump
      zoxide

      expect

      pciutils
      # adb for Android device access. Replaces programs.adb removed in 26.05,
      # systemd 258 handles udev/uaccess rules automatically.
      android-tools

      # command-line YAML processor, Used by the tmux
      # tmux-nerd-font-window-name.tmux plugin.
      yq

      # Used for fetching source from github
      # I used it to package tmux plugin that was not already available.
      nix-prefetch-github
      nix-prefetch-git

      # added to get access to lsusb
      usbutils

      # Used to list the process that that open a file or using a path as
      # a mounting point.
      lsof

      # Sound control
      pulseaudio # installed for pactl(which is cli for pavucontrol) and uses alsa.

      # Send notifications to the notification daemon, such as Dunst.
      libnotify

      # evalute key and mouse presses for xorg
      xev

      # Searching nix packages
      nix-search-cli # uses as "nix-search <name of package>"

      git-filter-repo
      iperf3
      dig
      buku
      wireguard-tools
      upower
      smartmontools # tool to assess hard drives(sudo smartctl -a /dev/sda).

      pdftk # pdf editing.

      # used for generatig ascii art in terminal(use showfigfonts to see all fonts)
      figlet
      # used for generatig ascii art same as figlet but with more fonts
      # pyfiglet -f ansi_shadow pi
      python313Packages.pyfiglet
      monero-cli
      jq
      dragon-drop
      imv
      tealdeer
      immich-go # used for mass importing into immich.
      pkgs-unstable.cursor-cli
      ddcutil
      pkgs-unstable.github-copilot-cli
      sshuttle
    ];
  };
}
