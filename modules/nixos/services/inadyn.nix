{ config, lib, ... }:
{
  config = lib.mkIf config.services.inadyn.enable {
    sops.secrets."inadyn/dynu" = {
      owner = config.services.inadyn.user;
      mode = "0600";
    };

    sops.secrets."inadyn/cloudflare" = {
      owner = config.services.inadyn.user;
      mode = "0600";
    };

    services.inadyn = {
      logLevel = "debug";
      settings = {
        period = 300;

        # Dynu stays custom (because it's NIC-style)
        custom.dynu = {
          include = config.sops.secrets."inadyn/dynu".path;
          ddns-server = "api.dynu.com";
          ddns-path = "/nic/update?username=%u&password=%p&myip=%i&hostname=%h";
        };

        # Cloudflare is a *provider*, not a custom section
        provider."cloudflare.com" = {
          include = config.sops.secrets."inadyn/cloudflare".path;
        };
      };
    };
  };
}
