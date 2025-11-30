{ config, lib, ... }:
{
  config = lib.mkIf config.programs.delta.enable {
    programs.delta = {
      enableGitIntegration = true;
      options = {
        navigate = true;
        line-numbers = true;
        side-by-side = false;
        theme = "Nord";
      };
    };
  };
}
