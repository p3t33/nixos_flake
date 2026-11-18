{ ... }:
{
  flake.modules.nixos.profile-system-services = { config, lib, ... }:
    let g = config.custom.profiles.systemServices; in
    {
      config = lib.mkIf g.xmr-miner.enable {
        services.monero.enable = true;
        custom.services.p2pool.enable = true;
        services.xmrig.enable = true;
      };
    };
}
