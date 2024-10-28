{ config, ... }:
{
  # Global settings for displaymanger(LightDM, SDDM, etc).
  services.displayManager = {
    defaultSession = "none+i3";
    autoLogin = {
      enable = true;
      user = config.userDefinedGlobalVariables.primeUsername;
    };
  };
}
