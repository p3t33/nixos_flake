{ ... }:
{
  flake.modules.homeManager.bash = { config, lib,  ... }:
{

  config = lib.mkIf config.programs.bash.enable {
    programs.bash = {
    };
  };
};
}

