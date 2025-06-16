{ config, ... }:
let
  serviceName = "jackett";
in
{
  services.${serviceName} = {
    enable = true;
    openFirewall = true;
    port = 9117;
    user = "${serviceName}";
    group = "${config.customGlobalOptions.mediaGroup}";
  };
}
