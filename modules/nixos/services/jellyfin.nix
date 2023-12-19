{ pkgs, ... }:
{
    environment.systemPackages = with pkgs; [
        jellyfin
        jellyfin-web
        jellyfin-ffmpeg
  ];

    services.jellyfin = {
        enable = true;
        openFirewall = true;
        group = "media";
    };
}
