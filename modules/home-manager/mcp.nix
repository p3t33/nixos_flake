{
  config,
  lib,
  osConfig,
  pkgs-unstable,
  ...
}:

{
  config = lib.mkIf config.programs.mcp.enable {
    custom.services.mcp-gateway.enable = true;

    programs.mcp.servers = {
      codegraph = {
        command = lib.getExe pkgs-unstable.codegraph;
        args = [ "serve" "--mcp" ];
      };

      mcp-gateway = {
        url = "http://${osConfig.custom.shared.localHostIPv4}:${toString config.custom.services.mcp-gateway.port}/mcp";
      };
    };
  };
}
