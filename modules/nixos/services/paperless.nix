{ config, ... }:
{
  sops.secrets.paperless-ngx = {};
  services.paperless = {
    enable = true;
    address = "${config.userDefinedGlobalVariables.anyIPv4}";
    port = config.userDefinedGlobalVariables.servicePort.paperless;
    passwordFile = config.sops.secrets.paperless-ngx.path;
  };


  networking.firewall.allowedTCPPorts = [ config.services.paperless.port ];
}
