{
  config,
  lib,
  osConfig,
  ...
}:

{
  config = lib.mkIf config.programs.mcp.enable {
    custom.services.mcp-gateway.enable = true;

    programs.mcp.servers.mcp-gateway = {
      url = "http://${osConfig.custom.shared.localHostIPv4}:${toString config.custom.services.mcp-gateway.port}/mcp";
    };
  };
}
