{ ... }:
{
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
}
