{ ... }:
{
  flake.modules.nixos.profile-system-services = { config, lib, ... }:
    let g = config.custom.profiles.systemServices; in
    {
      config = lib.mkIf g.monitoring.enable {
        services.prometheus.enable = true;
        services.loki.enable = true;
        services.alloy.enable = true;
        services.grafana.enable = true;
      };
    };
}
