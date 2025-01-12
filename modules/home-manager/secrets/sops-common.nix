{ config, ... }:
{
  sops.defaultSopsFile = config.userDefinedGlobalVariables.homeManagerAsNixOSModuleDefaultSecretsPath;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.userDefinedGlobalVariables.sopsKeyPath;
}
