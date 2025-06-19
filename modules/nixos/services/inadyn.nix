{ config, lib, ... }:
{
  config = lib.mkIf config.services.inadyn.enable {
    sops.secrets."inadyn-dynu" = {
      owner = config.services.inadyn.user;
      mode = "0600";
    };

    services.inadyn = {
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
