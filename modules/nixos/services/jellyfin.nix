{ pkgs, config, lib, ... }:
{
  options.custom = {
    servicePort.jellyfin = lib.mkOption {
      type = lib.types.int;
      default = 8096;
      description = "Jellyfin port";
    };
  };

  config = lib.mkIf config.services.jellyfin.enable {
    environment.systemPackages = with pkgs; [
      jellyfin
      jellyfin-web
      jellyfin-ffmpeg
    ];

    services.jellyfin = {
      openFirewall = true;
      group = "${config.custom.shared.mediaGroup}";
    };
  };
}
