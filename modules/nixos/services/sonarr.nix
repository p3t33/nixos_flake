{ config, ... }:
{
  # Enable the Sonarr service
  services.sonarr = {
    enable = true;
    openFirewall = true;                      # Opens Sonarr's port on the firewall (default 8989)
    user = "sonarr";
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
  };
}

