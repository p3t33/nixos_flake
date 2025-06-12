{ config, userDefinedGlobalVariables, ... }:
{
  sops.defaultSopsFile = userDefinedGlobalVariables.homeManagerAsNixOSModuleDefaultSecretsPath;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = userDefinedGlobalVariables.sopsKeyPath;
}
