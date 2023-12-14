{ config, ... }:
{
    sops.defaultSopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
    sops.defaultSopsFormat = "yaml";
    sops.age.keyFile = "${config.userDefinedGlobalVariables.homeDirectory}/.config/sops/age/keys.txt";
}
