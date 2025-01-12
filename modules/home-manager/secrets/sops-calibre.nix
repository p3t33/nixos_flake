{ config, ... }:
{
    sops.secrets.calibre = {
      format = "json";
      sopsFile = "${config.userDefinedGlobalVariables.secretsPath}/home-manager/calibre.json";
      path = "${config.userDefinedGlobalVariables.primeUserHomeDirectory}/.config/calibre/smtp.py.json";
      key = "";
    };
}

