{ config, lib, ... }:

let
  serviceName = "bazarr";
in
{
  config = lib.mkIf config.services.${serviceName}.enable {
    services.${serviceName} = {
      openFirewall = true;
      user = "${serviceName}";
      group = "${config.custom.shared.mediaGroup}";
      listenPort = 6767;
    };
  };
}
