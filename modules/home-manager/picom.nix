{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.picom;
in
{
  options.customOptions.enableModule.picom =
    lib.mkEnableOption "Enable picom compositor";

  config = lib.mkIf cfg {
    services.picom.enable = true;
  };
}
