{ config, ... }:
{
    imports = [
        ../../modules/nixos/security/sops/sops-common.nix
        ../../modules/nixos/security/sops/sops-syncthing.nix
    ];

    sops.secrets.ssh_id_ed25519_vm_key = {
        mode = "0600";
        owner = "${config.userDefinedGlobalVariables.username}";
        path = "${config.userDefinedGlobalVariables.homeDirectory}/.ssh/id_ed25519_vm_key";
    };

    sops.secrets.remotes = {
        sopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
    };

    sops.templates."extra_ssh_hosts" = {
        mode = "0600";
        owner = "${config.userDefinedGlobalVariables.username}";
        path = "${config.userDefinedGlobalVariables.homeDirectory}/.ssh/extra_hosts";
        content = ''
            ${config.sops.placeholder.remotes}
        '';
    };
}
