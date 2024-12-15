{ config, ... }:
{
  services.radarr = {
    enable = true;
    openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
    user = "radarr";
  };
}
