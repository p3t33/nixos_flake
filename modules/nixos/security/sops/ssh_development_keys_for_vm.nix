{ config, ... }:
{
    sops.secrets.ssh_id_ed25519_development_vm_private_key = {
        mode = "0600";
        owner = "${config.userDefinedGlobalVariables.primeUsername}";
        path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm";
    };

    sops.secrets.ssh_id_ed25519_development_vm_public_key = {
        mode = "0600";
        owner = "${config.userDefinedGlobalVariables.primeUsername}";
        path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.ssh/id_ed25519_development_vm.pub";
    };
}

