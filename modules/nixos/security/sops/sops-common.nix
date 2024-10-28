{ config, ... }:
{
  sops.defaultSopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.userDefinedGlobalVariables.sopsKeyPath;
}
