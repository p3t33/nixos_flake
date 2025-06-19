{ config, lib,  ... }:
{

  config = lib.mkIf config.programs.bash.enable {
    programs.bash = {
    };
  };
}

