{ config, ... }:
{
    sops.secrets."syncthing/cert.pem" = {
        sopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
        owner = "${config.userDefinedGlobalVariables.syncthingUser}";
        path = "${config.userDefinedGlobalVariables.syncthingConfigDirectory}/cert.pem";
        mode = "0600";

    };

    sops.secrets."syncthing/key.pem" = {
        sopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
        owner = "${config.userDefinedGlobalVariables.syncthingUser}";
        path = "${config.userDefinedGlobalVariables.syncthingConfigDirectory}/key.pem";
        mode = "0600";
    };
}
