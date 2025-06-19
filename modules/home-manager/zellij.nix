{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.zellij;
in
{
  options.customOptions.enableModule.zellij = lib.mkEnableOption "Enable Zellij terminal multiplexer";

  config = lib.mkIf cfg {
    programs.zellij = {
      enable = true;
    };
  };
}

