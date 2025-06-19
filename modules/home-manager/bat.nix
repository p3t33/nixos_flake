{ config, lib, ... }:

let
  cfg = config.customOptions.enableModule.bat;
in
{
  options.customOptions.enableModule.bat = lib.mkEnableOption "Enable bat (cat clone with syntax highlighting)";

  config = lib.mkIf cfg {
    programs.bat = {
      enable = true;
      config = {
        theme = "Visual Studio Dark+";
        wrap = "character";
        terminal-width = "80";
      };
    };
  };
}
