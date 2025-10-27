{ config, lib, ... }:
{
  config = lib.mkIf config.programs.tealdeer.enable {
    programs.tealdeer = {
      settings = {
        updates = {
          auto_update = true;
          auto_update_interval_hours = 24;
        };
      };
    };
  };
}
