{ config, lib, pkgs, ... }:
{

  config = lib.mkIf config.programs.steam.enable {

    programs = {
      steam = {
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
