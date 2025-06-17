{ config, ... }:
{
  sops.defaultSopsFile = config.customGlobalOptions.NixOSDefaultSecretsPath;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.customGlobalOptions.sopsKeyPath;
}
