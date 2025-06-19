{ config, lib, ... }:

let
  serviceName = "bazarr";
in
{
  config = lib.mkIf config.services.${serviceName}.enable {
    services.${serviceName} = {
      openFirewall = true;
      user = "${serviceName}";
      group = "${config.customGlobal.mediaGroup}";
      listenPort = 6767;
    };
  };
}
