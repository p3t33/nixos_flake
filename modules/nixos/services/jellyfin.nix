{ pkgs, config, lib, ... }:
{

  options.customOptions.servicePort.jellyfin = lib.mkOption {
    type = lib.types.int;
    default = 8096;
    description = "Jellyfin port";
  };

  config = {
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
