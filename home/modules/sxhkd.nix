{ pkgs, ... }:
{

  services.sxhkd = {
    enable = true;
    keybindings = {
      "super + p" = " ${pkgs.flameshot}/bin/flameshot gui";
      "Print" = "exec ${pkgs.flameshot}/bin/flameshot gui";
     };
   };

}
