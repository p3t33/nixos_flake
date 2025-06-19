{ config, lib, ... }:
{
  config = lib.mkIf config.services.openssh.enable {
    services.openssh = {
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
        # migh need to enable it for fido/fido2
        KbdInteractiveAuthentication = false;
      };
    };
  };
}
