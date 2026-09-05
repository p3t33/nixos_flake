{ config, lib, ... }:

let
  cfg = config.custom.security.acme;
  appsDomain = config.custom.shared.appsDomain;
in
{
  options.custom.security.acme.enable = lib.mkEnableOption "ACME certificates";

  config = lib.mkIf cfg.enable {
    sops.secrets."cloudflare/acme-api-token" = {};

    security.acme = {
      acceptTerms = true;

      certs.${appsDomain} = {
        domain = "*.${appsDomain}";
        server = "https://acme-v02.api.letsencrypt.org/directory";
        dnsProvider = "cloudflare";
        keyType = "rsa4096";
        group = config.services.nginx.group;
        credentialFiles."CF_DNS_API_TOKEN_FILE" = config.sops.secrets."cloudflare/acme-api-token".path;
      };
    };
  };
}
