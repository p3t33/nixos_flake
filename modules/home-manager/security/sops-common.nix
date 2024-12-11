{ config, ... }:
{
  sops.defaultSopsFile = config.userDefinedGlobalVariables.homeManagerAsNixOSModuleSecretsPath;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.userDefinedGlobalVariables.sopsKeyPath;
}
