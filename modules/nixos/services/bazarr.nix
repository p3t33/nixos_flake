{ config, ... }:
{
  # Enable the Sonarr service(as of now there is no config for default sonarr port)
  services.bazarr = {
    enable = true;
    openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
    user = "bazarr";
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
    listenPort = config.userDefinedGlobalVariables.servicePort.bazarr;
  };

}
