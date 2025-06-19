{ config, inputs, hostSpecific, ... }:
{
  sops = {
    defaultSopsFile = inputs.self + "/machines/${hostSpecific.hostName}/secrets/nixos/secrets.yaml";
    defaultSopsFormat = "yaml";
    age.keyFile = config.customGlobal.sopsKeyPath;
  };
}
