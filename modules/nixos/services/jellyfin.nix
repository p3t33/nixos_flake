{ pkgs, config, ... }:
{
  environment.systemPackages = with pkgs; [
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
  ];

  services.jellyfin = {
    enable = true;
    openFirewall = true;
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
  };
}
