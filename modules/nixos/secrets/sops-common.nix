{ config, ... }:
{
  sops.defaultSopsFile = config.userDefinedGlobalVariables.NixOSDefaultSecretsPath;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.userDefinedGlobalVariables.sopsKeyPath;
}
