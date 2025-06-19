{ config, lib, ... }:

let
  serviceName = "jackett";
in
{
  config = lib.mkIf config.services.${serviceName}.enable {
    services.${serviceName} = {
      openFirewall = true;
      port = 9117;
      user = "${serviceName}";
      group = "${config.customGlobal.mediaGroup}";
    };
  };
}
