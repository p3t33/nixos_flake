{ config, lib, ... }:
{
  config = lib.mkIf config.programs.zoxide.enable {
    programs.zoxide = {
      enableZshIntegration = true;
      enableBashIntegration = true;
      options = [ "--cmd j" ];
    };
  };
}
