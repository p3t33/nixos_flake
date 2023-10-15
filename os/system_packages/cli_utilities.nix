{ config, pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
      # presentation
      present
      slides

    # web
      wget
      speedtest-cli

    # process monitor
      htop

    # networking
      tcpdump
      nmap
      whois
      openconnect

    # search
      ripgrep
      fd
      fzf

   # system information
      inxi
      ncdu
      du-dust
      duf

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


      exa
      tree
      taskwarrior

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
    ];

}
