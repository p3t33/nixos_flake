{ config, ... }:
{
    sops.defaultSopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
    sops.defaultSopsFormat = "yaml";
    sops.age.keyFile = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.config/sops/age/keys.txt";
}
