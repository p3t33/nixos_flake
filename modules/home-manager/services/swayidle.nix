{ config, lib, ... }:
let
  swaymsg = lib.getExe' config.wayland.windowManager.sway.package "swaymsg";
in
{
  services.swayidle.timeouts = lib.optionals config.services.swayidle.enable [
    {
      timeout = 900;
      command = "${swaymsg} output '*' power off";
      resumeCommand = "${swaymsg} output '*' power on";
    }
  ];
}
