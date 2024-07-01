{ config, pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
      # presentation
      present
      slides
      haskellPackages.patat
      w3m

    # web
      wget
      speedtest-cli

    # process monitor
      htop
      yazi
      ueberzugpp

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
      du-dust
      duf

    # disk utils
      gptfdisk
      ntfs3g

    # general
      sshfs
      sshpass
      lsyncd
      rclone
      watchman
      restic
      bat
      unzip
      tmux
      smug # Session manager for tmux.
      moreutils # I need the sponge appliction to be used as part of tmux configuration.
      exfat # required by Ventoy installer.

      lshw


      eza
      tree
      #taskwarrior3

      zsh

      # A cross shell smarter cd command, inspired by z and autojump
      zoxide

      thefuck
      expect

      pciutils

      # command-line YAML processor, Used by the tmux
      # tmux-nerd-font-window-name.tmux plugin.
      yq

      # Used for fetching source from github
      # I used it to package tmux pluging that was not already available.
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
      xorg.xev

    ];

}
