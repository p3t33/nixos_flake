{ config, ... }:
{
  sops.secrets."git/userEmail" = {
    sopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
  };

  sops.secrets."git/userName" = {
    sopsFile = config.userDefinedGlobalVariables.NixOSSecretsPath;
  };

  sops.templates."git_credentials" = {
    mode = "0600";
    owner = "${config.userDefinedGlobalVariables.primeUsername}";
    path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.config/git/credentials";
    content = ''
      [user]
              email = "${config.sops.placeholder."git/userEmail"}"
              name = "${config.sops.placeholder."git/userName"}"
    '';
  };
}
