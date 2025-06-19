{ config, lib, pkgs, ... }:

let
  cfg = config.customOptions.enableModule.bash;
in
{
  options.customOptions.enableModule.bash =
    lib.mkEnableOption "Enable Bash shell support";

  config = lib.mkIf cfg {
    programs.bash = {
      enable = true;
    };
  };
}

