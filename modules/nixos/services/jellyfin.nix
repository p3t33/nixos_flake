{ pkgs, config, lib, ... }:
let
  cfg = config.customOptions.enableModule.jellyfin;
in
{
  options.customOptions = {
    enableModule.jellyfin = lib.mkEnableOption "Enable Jellyfin media server with firewall and media group support";

    servicePort.jellyfin = lib.mkOption {
      type = lib.types.int;
      default = 8096;
      description = "Jellyfin port";
    };
  };

  config = lib.mkIf cfg {
    environment.systemPackages = with pkgs; [
      jellyfin
      jellyfin-web
      jellyfin-ffmpeg
    ];

    services.jellyfin = {
      enable = true;
      openFirewall = true;
      group = "${config.customGlobalOptions.mediaGroup}";
    };
  };
}
