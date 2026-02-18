{ config, ... }:
{
  sops.secrets."development_vm_ssh_keys/private" = {
    path = "${config.custom.shared.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm";
  };

  sops.secrets."development_vm_ssh_keys/public" = {
    path = "${config.custom.shared.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm.pub";
  };
}
