{ hostSpecification, ... }:
{
  sops.secrets."development_vm_ssh_keys/private" = {
    path = "${hostSpecification.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm";
  };

  sops.secrets."development_vm_ssh_keys/public" = {
    path = "${hostSpecification.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm.pub";
  };
}
