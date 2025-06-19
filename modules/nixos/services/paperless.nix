{ config, lib, ... }:
{
  config = lib.mkIf config.services.paperless.enable {
    sops.secrets.paperless-ngx = {};
    services.paperless = {
      address = "${config.customGlobal.anyIPv4}";
      port = 28981;
      passwordFile = config.sops.secrets.paperless-ngx.path;
    };


    networking.firewall.allowedTCPPorts = [ config.services.paperless.port ];
  };
}
