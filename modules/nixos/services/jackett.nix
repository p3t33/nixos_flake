{ config, lib, ... }:

let
  serviceName = "jackett";
  cfg = config.customOptions.enableModule.${serviceName};
in
{
  options.customOptions.enableModule.${serviceName} = lib.mkEnableOption "Enable the Jackett indexer proxy";

  config = lib.mkIf cfg {
  services.${serviceName} = {
    enable = true;
    openFirewall = true;
    port = 9117;
    user = "${serviceName}";
    group = "${config.customGlobalOptions.mediaGroup}";
  };
  };
}
