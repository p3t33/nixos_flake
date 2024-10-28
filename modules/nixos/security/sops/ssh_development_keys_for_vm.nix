{ config, ... }:
{
  sops.secrets."development_vm_ssh_keys/private" = {
    mode = "0600";
    owner = "${config.userDefinedGlobalVariables.primeUsername}";
    path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm";
  };

  sops.secrets."development_vm_ssh_keys/public" = {
    mode = "0600";
    owner = "${config.userDefinedGlobalVariables.primeUsername}";
    path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm.pub";
  };
}
