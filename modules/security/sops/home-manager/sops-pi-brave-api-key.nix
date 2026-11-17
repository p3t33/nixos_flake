{ ... }:
{
  flake.modules.homeManager.sops-pi-brave-api-key = { config, ... }:
{
  sops.secrets."pi-agent/brave-api-key" = { };

  custom.programs.pi.webSearch.braveApiKeyFile = config.sops.secrets."pi-agent/brave-api-key".path;
}
;
}
