{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.custom.programs.whatsapp;
in
{
  options.custom.programs.whatsapp.enable = lib.mkEnableOption "WhatsApp Web in Google Chrome app mode";

  config = lib.mkIf cfg.enable {
    xdg.desktopEntries.whatsapp = {
      name = "WhatsApp";
      comment = "Open WhatsApp Web in a standalone window";
      exec = "${lib.getExe' pkgs.google-chrome "google-chrome-stable"} --app=https://web.whatsapp.com/";
      icon = "${pkgs.papirus-icon-theme}/share/icons/Papirus/48x48/apps/whatsapp.svg";
      terminal = false;
      categories = [
        "Network"
        "InstantMessaging"
      ];
    };
  };
}
