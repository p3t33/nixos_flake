# Remote HTTP backend notes:
#
# 1. Many remote MCP servers use Streamable HTTP, not legacy SSE.
#    Set `streamable_http = true` per backend when SSE connection fails.
#
# 2. Auth tokens via env vars: set `headers.Authorization = "env:VAR_NAME"`
#    where VAR_NAME is defined in the gateway's EnvironmentFile (e.g. sops
#    secret). Value format: `Bearer <token>`.
#
# 3. Protocol version pinning: mcp-gateway may default to a newer MCP
#    protocol version than the server supports. Pin per backend with
#    `protocol_version = "<version>"`. Check server error for supported
#    versions.
#
# Example backend:
#   backends.example = {
#     http_url = "https://mcp.example.com/servers/foo";
#     description = "Example service";
#     headers.Authorization = "env:MCP_EXAMPLE_TOKEN";
#     streamable_http = true;
#     protocol_version = "2025-06-18";
#   };

{
  config,
  lib,
  pkgs-unstable,
  osConfig,
  ...
}:

let
  cfg = config.custom.services.mcp-gateway;
  yamlFormat = pkgs-unstable.formats.yaml { };

  backendToYaml = name: backend:
    if backend.http_url != null then
      { inherit (backend) description; http_url = backend.http_url; }
      // lib.optionalAttrs (backend.headers != { }) { inherit (backend) headers; }
      // lib.optionalAttrs (backend.streamable_http) { streamable_http = true; }
      // lib.optionalAttrs (backend.protocol_version != null) { inherit (backend) protocol_version; }
    else
      { inherit (backend) description command; }
      // lib.optionalAttrs (backend.env != { }) { inherit (backend) env; };

  transformMcpServer = name: server:
    let
      isDisabled = server.disabled or false;
      hasUrl = server ? url;
      hasCommand = server ? command;
      hasBoth = hasUrl && hasCommand;
      hasNeither = !hasUrl && !hasCommand;
    in
    if isDisabled then null
    else if hasBoth then
      builtins.trace "mcp-gateway: skipping '${name}' — has both url and command" null
    else if hasNeither then
      builtins.trace "mcp-gateway: skipping '${name}' — has neither url nor command" null
    else
      lib.optionalAttrs hasUrl {
        http_url = server.url;
      }
      // lib.optionalAttrs hasCommand {
        command = lib.concatStringsSep " " (
          [ server.command ] ++ map lib.escapeShellArg (server.args or [])
        );
      }
      // lib.optionalAttrs (server ? headers) {
        inherit (server) headers;
      }
      // lib.optionalAttrs (server ? env) {
        inherit (server) env;
      }
      // {
        description = server.description or name;
      };

  transformedMcpServers =
    if cfg.enableMcpIntegration && config.programs.mcp.enable then
      lib.filterAttrs (_: v: v != null) (
        lib.mapAttrs transformMcpServer (
          lib.filterAttrs (k: _: !(lib.elem k cfg.excludeMCPs)) config.programs.mcp.servers
        )
      )
    else
      { };

  gatewayConfig = {
    server = {
      host = osConfig.custom.shared.localHostIPv4;
      port = cfg.port;
    };
    meta_mcp = {
      enabled = true;
      cache_tools = true;
      cache_ttl = "300s";
    };
    backends = transformedMcpServers // lib.mapAttrs backendToYaml cfg.backends;
  };
in
{
  options.custom.services.mcp-gateway = {
    enable = lib.mkEnableOption "MCP Gateway — universal MCP server multiplexer";

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to an environment file (e.g. sops secret) with variables referenced via env: prefix in backend config.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 39400;
      description = "Port the gateway listens on.";
    };

    enableMcpIntegration = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Merge servers from {option}`programs.mcp.servers` into gateway
        backends. Servers with `disabled = true` are excluded.
      '';
    };

    excludeMCPs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "mcp-gateway" ];
      description = ''
        Server names to exclude when importing from
        {option}`programs.mcp.servers`. Prevents self-referential config
        when mcp-gateway itself appears in the server list.
      '';
    };

    backends = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          description = lib.mkOption {
            type = lib.types.str;
            default = "";
            description = "Human-readable description of this backend.";
          };

          # Remote HTTP backend
          http_url = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "URL for remote HTTP/SSE MCP server. Mutually exclusive with command.";
          };

          headers = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = { };
            description = "HTTP headers (e.g. Authorization) for remote backends.";
          };

          streamable_http = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Use Streamable HTTP transport (no SSE handshake).";
          };

          # Local stdio backend
          command = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Command to spawn a local stdio MCP server. Mutually exclusive with http_url.";
          };

          env = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = { };
            description = "Environment variables passed to spawned command.";
          };

          protocol_version = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Override MCP protocol version (e.g. '2025-06-18'). Auto-negotiated if null.";
          };
        };
      });
      default = { };
      description = "MCP backend servers to route through the gateway.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = lib.mapAttrsToList (name: backend: {
      assertion = (backend.http_url != null) != (backend.command != null);
      message = "MCP gateway backend '${name}' must set exactly one of http_url or command.";
    }) cfg.backends;

    home.packages = [ pkgs-unstable.mcp-gateway ];

    xdg.configFile."mcp-gateway/gateway.yaml" = {
      source = yamlFormat.generate "gateway.yaml" gatewayConfig;
    };

    systemd.user.services.mcp-gateway = {
      Unit = {
        Description = "MCP Gateway";
        After = [ "network.target" ] ++ lib.optionals (cfg.environmentFile != null) [ "sops-nix.service" ];
        Wants = lib.optionals (cfg.environmentFile != null) [ "sops-nix.service" ];
      };

      Service = {
        Type = "simple";
        ExecStart = "${lib.getExe pkgs-unstable.mcp-gateway} serve --config %h/.config/mcp-gateway/gateway.yaml";
        Restart = "on-failure";
        RestartSec = 5;
      } // lib.optionalAttrs (cfg.environmentFile != null) {
        EnvironmentFile = cfg.environmentFile;
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
