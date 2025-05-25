{ config, pkgs,... }:
let
  serviceName = "bazarr";
in
{
  # Enable the Sonarr service(as of now there is no config for default sonarr port)
  services.${serviceName} = {
    enable = true;
    openFirewall = true; # Opens Sonarr's port on the firewall (default 8989)
    user = "${serviceName}";
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
    listenPort = 6767;
  };
}
