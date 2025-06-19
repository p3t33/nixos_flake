{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.greenclip;
in
{
  options.customOptions.enableModule.greenclip = lib.mkEnableOption "Enable Greenclip clipboard manager";

  config = lib.mkIf cfg {
    services.greenclip.enable = true;
  };
}
