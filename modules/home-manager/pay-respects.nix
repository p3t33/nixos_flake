{ config, lib, ... }:
{
  config = lib.mkIf config.programs.pay-respects.enable {
    programs.pay-respects = {
      enableZshIntegration = true;
    };
  };
}

