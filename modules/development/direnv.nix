{ ... }:
{
  flake.modules = {
    homeManager.direnv = { config, lib, ... }:
      {
        config = lib.mkIf config.programs.direnv.enable {
          programs.direnv = {
            enableZshIntegration = true;
            nix-direnv.enable = true;
          };
        };
      };
  };
}
