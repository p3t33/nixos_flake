{ config, lib, ... }:
let
  cfg = config.custom.services.homeAssistant;

  notifyPlugs = lib.concatMap
    (room: lib.filter (plug: plug.notifyWhenDone) (lib.attrValues room.plugs))
    (lib.attrValues cfg.rooms);

  # Triggers when power stays below 5W for 5 minutes — indicates cycle is finished.
  notifyAutomation = plug: {
    alias   = "Notify when ${plug.name} done";
    trigger = [{
      platform    = "numeric_state";
      entity_id   = plug.power;
      below       = 5;
      for.minutes = 5;
    }];
    action = [{
      # Use persistent_notification until mobile app service name is known.
      # Replace with notify.mobile_app_<device> once the app is paired.
      service = "notify.persistent_notification";
      data    = {
        title   = "${plug.name} finished";
        message = "${plug.name} cycle is complete.";
      };
    }];
  };

in
{
  config = lib.mkIf config.services.home-assistant.enable {
    services.home-assistant.config.automation =
      map notifyAutomation notifyPlugs;
  };
}
