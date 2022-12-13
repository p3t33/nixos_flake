{ ... }:
{

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enableSshSupport = true;
    enable = true;
    pinentryFlavor = "qt";
  };

}
