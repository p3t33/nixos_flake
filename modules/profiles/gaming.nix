{ ... }:
{
  flake.modules.nixos.profile-system = { config, lib, ... }:
    let g = config.custom.profiles.system; in
    {
      config = lib.mkIf g.gaming.enable {
        programs.steam.enable = true;
      };
    };
}
