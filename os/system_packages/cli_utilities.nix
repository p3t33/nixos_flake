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

    # display
      xorg.xrandr

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
      lsyncd
      rclone
      restic
      bat
      unzip
      tmux
      smug # Session manager for tmux.

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
    ];

}
