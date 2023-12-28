{ ... }:
{
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
  };
}
