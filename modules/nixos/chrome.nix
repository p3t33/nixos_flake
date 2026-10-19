{ config, lib, pkgs, ... }:

let
  cfg = config.custom.apps.chrome;
in
{
  options.custom.apps.chrome.enable = lib.mkEnableOption "Google Chrome with Nord browser policies";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.google-chrome ];

    programs.chromium = {
      enable = true;
      extensions = [
        "ddkjiahejlhfcafbddmgiahcphecmpfh" # uBlock Origin Lite
        "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # Privacy Badger
        "ffemldjbbfhimggdkkckolidemlllklk" # Mooltipass
        "imohnlganmafcmidafklgkgfgaagiohn" # Yet Another Speed Dial
        "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
        "pccckmaobkjjboncdfnnofkonhgpceea" # Hover Zoom+
        "neebplgakaahbhdphmkckjjcegoiijjo" # Keepa
        "iaiomicjabeggjcfkbimgmglanimpnae" # Tab Session Manager
      ];
      extraOpts = {
        BrowserThemeColor = "#2e3440";
      };
    };
  };
}
