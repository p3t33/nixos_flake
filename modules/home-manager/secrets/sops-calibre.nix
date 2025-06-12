{ config, hostSpecification, userDefinedGlobalVariables, ... }:
{
    sops.secrets.calibre = {
      format = "json";
      sopsFile = "${userDefinedGlobalVariables.secretsPath}/home-manager/calibre.json";
      path = "${hostSpecification.primeUserHomeDirectory}/.config/calibre/smtp.py.json";
      key = "";
    };
}

