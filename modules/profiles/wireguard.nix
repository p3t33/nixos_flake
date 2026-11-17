{ ... }:
{
  flake.modules.nixos.profile-system-services = { config, lib, ... }:
    let g = config.custom.profiles.systemServices; in
    {
      config = lib.mkIf g.wireguardServer.enable {
        custom.vpn.wireguardServer.enable = true;
        # as inadyn is no longer maintained(and so has been deleted) if
        # this profile to be enabled a new dynamic dns service is required to
        # be configured.
      };
    };
}
