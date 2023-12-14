{ config, ... }:
{
    sops.secrets."syncthing/cert.pem" = {
        sopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
        owner = "${config.userDefinedGlobalVariables.username}";
        path = "${config.userDefinedGlobalVariables.homeDirectory}/.config/syncthing/cert.pem";
        mode = "0600";

    };

    sops.secrets."syncthing/key.pem" = {
        sopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
        owner = "${config.userDefinedGlobalVariables.username}";
        path = "${config.userDefinedGlobalVariables.homeDirectory}/.config/syncthing/key.pem";
        mode = "0600";
    };
}
