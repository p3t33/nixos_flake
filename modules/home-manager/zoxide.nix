{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.zoxide;
in
{
  options.customOptions.enableModule.zoxide = lib.mkEnableOption "Enable zoxide, a smarter cd command";

  config = lib.mkIf cfg {
    programs.zoxide = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
      options = [ "--cmd j" ];
    };
  };
}
