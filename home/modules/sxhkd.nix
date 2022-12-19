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
   #    inherit the user environment which means that it isn'tvery good
   #    choise for deamons that are used to execute other executables(E.g tmux/sxhkd).
   #    this is why I only use to home-manger to create settings file for sxhkd and
   #    nothing else.
   # 2. Execute the binary from the i3wm, which is what I will be doing.
   # 

   home.file = {
     ".config/sxhkd/sxhkdrc".text = ''

       super + x
         rofi -modi ssh -show ssh

       super + z
         rofi -modi emoji -show emoji

       super + c
         rofi -modi calc -show calc
       
       super + p
         flameshot gui

       Print
         flameshot gui
       '';
   };

}
