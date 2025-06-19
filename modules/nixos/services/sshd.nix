{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.sshd;
in
{
  options.customOptions.enableModule.sshd =
    lib.mkEnableOption "Enable OpenSSH service";

  config = lib.mkIf cfg {
    # Enable the OpenSSH daemon.
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
        # migh need to enable it for fido/fido2
        KbdInteractiveAuthentication = false;
      };
    };
  };
}
