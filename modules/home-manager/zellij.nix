{ config, lib, ... }:

{

  config = lib.mkIf config.programs.zellij.enable {
    programs.zellij = {
      # enable = true;
    };
  };
}

