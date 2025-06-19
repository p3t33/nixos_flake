{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.fail2ban;
in
{
  options.customOptions.enableModule.fail2ban = lib.mkEnableOption "Enable fail2ban to ban malicious IPs after failed login attempts";

  config = lib.mkIf cfg {
    services.fail2ban = {
      enable = true;
    };
  };
}
