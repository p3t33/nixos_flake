{ config, lib, ... }:

{
  config = lib.mkIf config.programs.mcp.enable {
    programs.mcp.servers = { };
  };
}
