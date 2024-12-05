 {pkgs, ... }:

{

  programs.steam.enable = true;
  programs.steam.gamescopeSession.enable = true;

  environment.systemPackages = with pkgs; [
    mangohud
    protonup # GE-proton.
  ];

  programs.gamemode.enable = true;

  # Ensures that steam can recognize and use custom proton versions(like GE-proton).
  environment.sessionVariables = {
    STEAM_EXTRA_COMPAT_TOOLS_PATHS =
      "\${HOME}/.steam/root/compatibilitytools.d";
  };

}
