{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:

let
  cfg = config.custom.programs.pi;
in
{
  options.custom.programs.pi = {
    extensions = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.lines lib.types.path);
      default = { };
      description = ''
        Custom extensions as an attrset of `extensionName = inlineContent | /path/to/extension.ts`.
        Extensions are stored in .pi/agent/extensions/<name>.ts.
      '';
    };

  };

  config = lib.mkIf cfg.enable {
    custom.services.mcp-gateway.enable = true;

    home.file = lib.mapAttrs' (
      name: content:
      lib.nameValuePair ".pi/agent/extensions/${name}.ts" (
        if lib.isPath content then { source = content; } else { text = content; }
      )
    ) cfg.extensions;

    custom.programs.pi = {
      extensions = {
        fetch-url = import ./fetch-url.nix { inherit lib pkgs; };
        learning-planner = import ./learning-planner.nix;
        parse-document = import ./parse-document.nix { inherit lib pkgs; };
        mcp-adapter = import ./mcp-adapter.nix {
          port = config.custom.services.mcp-gateway.port;
          host = osConfig.custom.shared.localHostIPv4;
        };
      };
    };
  };
}
