{ config, inputs, hostSpecific, ... }:
{
  sops.defaultSopsFile = inputs.self + "/machines/${hostSpecific.hostName}/secrets/home-manager/secrets.yaml";
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.customGlobalOptions.sopsKeyPath;
}
