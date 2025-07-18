{ config, lib, ... }:
let
  cfg = config.customOptions.enableModule.sound;
in
{
  options.customOptions.enableModule.sound = lib.mkEnableOption "Enable PipeWire sound system configuration";

  config = lib.mkIf cfg {
    # Enable sound
    # sound.enable = true;
    #need to be false or it will conflict with PipeWire..
    services.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      # Used with pavucontrol
      pulse.enable = true;

      # If you want to use JACK applications, uncomment t    his
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };
  };
}
