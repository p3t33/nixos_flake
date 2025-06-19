{ config, ... }:
{
    sops.secrets.calibre = {
      format = "json";
      sopsFile = "${config.customGlobal.secretsPath}/home-manager/calibre.json";
      path = "${config.customGlobal.primeUserHomeDirectory}/.config/calibre/smtp.py.json";
      key = "";
    };
}

