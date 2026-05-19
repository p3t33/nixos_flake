{ config, inputs, hostSpecific, ... }:
{
  sops = {
    defaultSopsFile = inputs.self + "/machines/${hostSpecific.hostName}/secrets/nixos/secrets.yaml";
    defaultSopsFormat = "yaml";
    age.keyFile = config.custom.shared.sopsKeyPath;
    # We use dedicated age keys only; don't try to derive age keys from SSH
    # host keys. Prevents failures during nixos-install when SSH keys don't
    # exist yet.
    age.sshKeyPaths = [];
    gnupg.sshKeyPaths = [];
  };

  systemd.tmpfiles.rules = [
    "z ${config.custom.shared.sopsKeyPath} 0640 root sops-keys - -"
  ];
}
