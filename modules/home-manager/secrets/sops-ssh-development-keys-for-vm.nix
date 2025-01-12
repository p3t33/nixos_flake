{ config, ... }:
{
  sops.secrets."development_vm_ssh_keys/private" = {
    path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm";
  };

  sops.secrets."development_vm_ssh_keys/public" = {
    path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm.pub";
  };
}
