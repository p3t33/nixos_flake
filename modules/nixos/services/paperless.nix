{ config, ... }:
{
  sops.secrets.paperless-ngx = {};
  services.paperless = {
    enable = true;
    address = "${config.customGlobalOptions.anyIPv4}";
    port = 28981;
    passwordFile = config.sops.secrets.paperless-ngx.path;
  };


  networking.firewall.allowedTCPPorts = [ config.services.paperless.port ];
}
