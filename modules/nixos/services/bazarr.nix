{ config, pkgs, lib, ... }:

let
  serviceName = "bazarr";
  cfg = config.customOptions.enableModule.${serviceName};
in
{
  options.customOptions.enableModule.${serviceName} = lib.mkEnableOption "Enable Bazarr subtitle management service";

  config = lib.mkIf cfg {
    services.${serviceName} = {
      enable = true;
      openFirewall = true;
      user = "${serviceName}";
      group = "${config.customGlobalOptions.mediaGroup}";
      listenPort = 6767;
    };
  };
}
