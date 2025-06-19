{ config, lib, ... }:
{
  # todo, provide basic configurations.
  config = lib.mkIf config.services.fail2ban.enable {
    services.fail2ban = {
      extraPackages = [];
    };
  };
}
