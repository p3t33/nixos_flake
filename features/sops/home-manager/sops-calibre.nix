{ ... }:
{
  flake.modules.homeManager.sops-calibre = { config, ... }:
{
    sops.secrets.calibre = {
      format = "json";
      sopsFile = "${config.custom.shared.secretsPath}/home-manager/calibre.json";
      path = "${config.custom.shared.primeUserHomeDirectory}/.config/calibre/smtp.py.json";
      key = "";
    };
}
;
}
