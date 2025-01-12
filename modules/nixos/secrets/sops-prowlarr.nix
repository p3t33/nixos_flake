{ config, ... }:
{
  # created under /run/secrets/prowlarr-db
  # not that I replaced prowlarr.db with prowlarr-db although the file named
  # prowlarr.db
  # sops.secrets.prowlarr-db = {
  #   format = "binary";
  #   sopsFile = config.userDefinedGlobalVariables.databaseSecret;
  # };

  sops.secrets.prowlarr-db = {
    format = "binary";
    sopsFile = "${config.userDefinedGlobalVariables.secretsPath}/nixos/prowlarr/prowlarr.db";
  };
}
