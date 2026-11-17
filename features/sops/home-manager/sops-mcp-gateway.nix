{ ... }:
{
  flake.modules.homeManager.sops-mcp-gateway = { config, ... }:
{
  sops.secrets.mcp-gateway = { };

  custom.services.mcp-gateway.environmentFile = config.sops.secrets.mcp-gateway.path;
}
;
}
