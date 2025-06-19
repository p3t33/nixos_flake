{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.paperless;
in
{
  options.customOptions.enableModule.paperless = lib.mkEnableOption "Enable paperless-ngx service";

  config = lib.mkIf cfg {
    sops.secrets.paperless-ngx = {};
    services.paperless = {
      enable = true;
      address = "${config.customGlobalOptions.anyIPv4}";
      port = 28981;
      passwordFile = config.sops.secrets.paperless-ngx.path;
    };


    networking.firewall.allowedTCPPorts = [ config.services.paperless.port ];
  };
}
