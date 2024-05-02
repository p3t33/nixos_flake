{ config, ... }:
{
    sops.secrets.remotes = {
        sopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
    };

    sops.templates."extra_ssh_hosts" = {
        mode = "0600";
        owner = "${config.userDefinedGlobalVariables.primeUsername}";
        path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.ssh/extra_hosts";
        content = ''
            ${config.sops.placeholder.remotes}
        '';
    };
}
