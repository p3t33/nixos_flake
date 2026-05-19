{ config, inputs, hostSpecific, ... }:
{
  sops = {
    defaultSopsFile = inputs.self + "/machines/${hostSpecific.hostName}/secrets/nixos/secrets.yaml";
    defaultSopsFormat = "yaml";
    age.keyFile = config.custom.shared.sopsKeyPath;
  };

  systemd.tmpfiles.rules = [
    "z ${config.custom.shared.sopsKeyPath} 0640 root sops-keys - -"
  ];
}
