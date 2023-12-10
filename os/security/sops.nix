{ config, ... }:
{
    sops.defaultSopsFile = ../../secrets/secrets.yaml;
    sops.defaultSopsFormat = "yaml";
    sops.age.keyFile = "${config.userDefinedGlobalVariables.homeDirectory}/.config/sops/age/keys.txt";

    sops.secrets.dev-one = { };

    sops.templates."extra_ssh_hosts" = {
        mode = "0600";
        owner = "${config.userDefinedGlobalVariables.username}";
        path = "${config.userDefinedGlobalVariables.homeDirectory}/.ssh/extra_hosts";
        content = ''
            ${config.sops.placeholder.dev-one}
        '';

    };

    sops.secrets.ssh_private_key_vm = {
        owner = "${config.userDefinedGlobalVariables.username}";
        path = "${config.userDefinedGlobalVariables.homeDirectory}/.ssh/id_ed25519_vm_key";
    };
}
