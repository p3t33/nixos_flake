{ config, inputs, hostSpecific, ... }:
{
  sops.defaultSopsFile = inputs.self + "/machines/${hostSpecific.hostName}/secrets/home-manager/secrets.yaml";
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = config.custom.shared.sopsKeyPath;
  # We use dedicated age keys only; don't try to derive age keys from SSH
  # host keys. Prevents failures during nixos-install when SSH keys don't
  # exist yet.
  sops.age.sshKeyPaths = [];
  sops.gnupg.sshKeyPaths = [];

  home.sessionVariables.SOPS_AGE_KEY_FILE = config.custom.shared.sopsKeyPath;
}
