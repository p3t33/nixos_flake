{ config, lib, ... }:
{
  config = lib.mkIf config.programs.bat.enable {
    programs.bat = {
      config = {
        theme = "Visual Studio Dark+";
        wrap = "character";
        terminal-width = "80";
      };
    };
  };
}
