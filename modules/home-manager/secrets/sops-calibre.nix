{ config, ... }:
{
    sops.secrets.calibre = {
      format = "json";
      sopsFile = "${config.customGlobalOptions.secretsPath}/home-manager/calibre.json";
      path = "${config.customGlobalOptions.primeUserHomeDirectory}/.config/calibre/smtp.py.json";
      key = "";
    };
}

