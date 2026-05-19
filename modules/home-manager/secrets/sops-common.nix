{ config, inputs, hostSpecific, ... }:
{
  sops.defaultSopsFile = inputs.self + "/machines/${hostSpecific.hostName}/secrets/home-manager/secrets.yaml";
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.custom.shared.sopsKeyPath;

  home.sessionVariables.SOPS_AGE_KEY_FILE = config.custom.shared.sopsKeyPath;
}
