{ config, pkgs,... }:
let
  serviceName = "bazarr";
in
{
  services.${serviceName} = {
    enable = true;
    openFirewall = true;
    user = "${serviceName}";
    group = "${config.customGlobalOptions.mediaGroup}";
    listenPort = 6767;
  };
}
