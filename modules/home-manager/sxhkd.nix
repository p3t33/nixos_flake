{ pkgs, ... }:
{

  # Setting sxhkd as a service.
  # ---------------------------
  #
  # There a few ways to autostart sxhkd as a daemon.
  #
  # 1. Using systemd. This can be done by writing a user service
  #    that will go into configuration.nix(systemd.user.services.sxhkd).
  #    It can also be done by using the home-manger(services.sxhkd.enable = true;)
  #    This will create a scoped service. For some reason using home-manger doesn't work for me.
  #
  #    systemd service is intended to work in isolation, it doesn't inherit the user environment which means
  #    it isn"t a straight forward choice for daemons that are used to execute other executable(E.g emacs/tmux/sxhkd).
  #    This means that extra work need to be done to make them work wihch I did in my declared systemd service.

  # 2. Execute the binary from the i3wm, which couples a deamon to i3wm and does not provide
  #    the contorl and the logging that I can get with systemd service.
  #
  # So this file only handles the keybindings for the sxhkd.
  #
  # Notes for rofi key bindings
  # ---------------------------
  # Once available modes are defined in the rofi config.rasi executing a
  # model can be written in a short manner(E.g "rofi -show ssh" instead of
  # "rofi -modi ssh -show ssh") The reason I decided to use the longer version
  # is in order to decouple config.rasi from the rofi command. Because if a
  # model is removed from the config.rasi and a shorter version is used it will
  # brake.
  #
  # Also non builtin modes(just executable) need a bit of a different syntax
  # ssh, emoji, and calc are builtin modes while power-menu and buku-bookmarks
  # are not.

  home.file = {
    ".config/sxhkd/sxhkdrc".text = ''

      super + x
        rofi -modi ssh -show ssh

      super + z
        rofi -modi emoji -show emoji

      super + c
        rofi -modi "clipboard:greenclip print" -show clipboard -run-command '{cmd}'

      super + y
        rofi -modi calc -show calc

      super + p
        rofi -modi power-menu:rofi-power-menu -show power-menu

      super + b
        rofi -modi buku-bookmarks:rofi-buku-bookmakrs -show buku-bookmarks

      super + m
         emacsclient -c -e "(org-roam-dailies-capture-today)"

      Print
        flameshot gui

      XF86AudioRaiseVolume
        amixer sset Master 5%+

      XF86AudioLowerVolume
        amixer sset Master 5%-

      XF86AudioMute
        amixer sset Master toggle

      XF86AudioMicMute
        amixer set Capture toggle
    '';
  };
}
