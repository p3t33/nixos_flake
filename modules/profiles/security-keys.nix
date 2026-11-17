{ ... }:
{
  flake.modules.nixos.profile-system = { config, lib, ... }:
    let g = config.custom.profiles.system; in
    {
      config = lib.mkIf g.securityKeys.enable {
        custom.security.nitrokey.enable = true;
        custom.security.solokey2.enable = true;
      };
    };
}
