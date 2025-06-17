{ config, ... }:
{
  sops.defaultSopsFile = config.customGlobalOptions.homeManagerAsNixOSModuleDefaultSecretsPath;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.customGlobalOptions.sopsKeyPath;
}
