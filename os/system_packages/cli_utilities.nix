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
      restic
      bat
      unzip
      tmux
      smug # Session manager for tmux.
      moreutils # I need the sponge appliction to be used as part of tmux configuration.
      exfat # required by Ventoy installer.


      exa
      tree
      taskwarrior

      zsh
     # Backend that is used by oh-my-zsh z plugin.
     # an alternative is to install autojump
     # to use the plugin exezute "z" from the shell.
      zsh-z

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
    ];

}
