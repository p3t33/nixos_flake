{ config, lib, ... }:
let
  serviceName = "inadyn";
  cfg = config.customOptions.enableModule.${serviceName};
in
{
  options.customOptions.enableModule.${serviceName} = lib.mkEnableOption "Enable inadyn DDNS client with Dynu configuration";

  config = lib.mkIf cfg {
    sops.secrets."inadyn-dynu" = {
      owner = config.services.inadyn.user;
      mode = "0600";
    };

    services.inadyn = {
      enable = true;
      logLevel = "debug";
      settings = {
         custom.dynu = {
          include = config.sops.secrets.inadyn-dynu.path;
           ddns-server = "api.dynu.com";
           ddns-path = "/nic/update?username=%u&password=%p&myip=%i&hostname=%h"; # Correct path with variables
        };
      };
    };
  };
}
