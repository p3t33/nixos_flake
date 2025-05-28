{ config, pkgs,... }:
let
  serviceName = "bazarr";
in
{
  services.${serviceName} = {
    enable = true;
    openFirewall = true;
    user = "${serviceName}";
    group = "${config.userDefinedGlobalVariables.mediaGroup}";
    listenPort = 6767;
  };
}
