{ config, lib, pkgs, ... }:

let
  cfg = config.customOptions.enableModule.steamGaming;
in
{
  options.customOptions.enableModule.steamGaming = lib.mkEnableOption "Enable Steam and gaming tools like MangoHud, ProtonUp, and GameMode";

  config = lib.mkIf cfg {

    programs = {
      steam = {
        enable = true;
        gamescopeSession.enable = true;
      };

      gamemode.enable = true;
    };

    environment.systemPackages = with pkgs; [
      mangohud
      protonup # GE-proton.
    ];

    # Ensures that steam can recognize and use custom proton versions(like GE-proton).
    environment.sessionVariables = {
      STEAM_EXTRA_COMPAT_TOOLS_PATHS = "\${HOME}/.steam/root/compatibilitytools.d";
    };
  };
}
