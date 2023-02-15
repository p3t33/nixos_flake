{ pkgs, ... }:
{

   # Setting sxhkd as a service.
   # ---------------------------
   #
   # There a few ways to autostart sxhkd as
   # a daemon.
   # 
   # 1. Using systemd. This can be done by writing a user service
   #    that will go into configuration.nix(systemd.user.services.sxhkd).
   #    It can also be done by using the home-manger(services.sxhkd.enable = true;)
   #    For some reason using home-manger doesn't work for me. unit file isn't
   #    being created. But the problem with systemd services is that it 
   #    by design intended to work in isolation, it doesn't 
   #    inherit the user environment which means that it isn't very good
   #    choise for deamons that are used to execute other executables(E.g tmux/sxhkd).
   #    this is why I only use to home-manger to create settings file for sxhkd and
   #    nothing else.
   # 2. Execute the binary from the i3wm, which is what I will be doing.
   # 
   #
   # Notes for rofi key bindings
   # ---------------------------
   # Once available modes are defined in the rofi config.rasi executing a 
   # model can be written in a short manner(E.g "rofi -show ssh" instead of
   # "rofi -modi ssh -show ssh") The reason I decided to use the longer version
   # is in order to decapule config.rasi from the rofi command. Because if a 
   # model is removed from the config.rasi and a shorter version is used it will
   # brake.
   #
   # Also non builtin modes(just executables) need a bit of a different syntax
   # ssh, emoji, and calc are builtin modes while power-menu and firefox-bookmarks
   # are not. 

   home.file = {
     ".config/sxhkd/sxhkdrc".text = ''

       super + x
         rofi -modi ssh -show ssh

       super + z
         rofi -modi emoji -show emoji

       super + c
         rofi -modi calc -show calc
       
       super + p
         rofi -modi power-menu:rofi-power-menu -show power-menu

       super + b
         rofi -modi firefox-bookmarks:rofi-firefox-bookmakrs -show firefox-bookmarks

       Print
         flameshot gui
       '';
   };

}
