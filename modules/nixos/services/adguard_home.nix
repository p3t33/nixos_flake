{ config, lib, hostSpecific, ... }:
let
  dnsPort = 53;
  hostIP = config.custom.shared.${hostSpecific.hostName}.ip;

  # To add a DNS rewrite for a new service, append an entry here.
  # The domain defaults to "${name}.${hostName}" but can be overridden.
  rewriteEntries = [
    { name = "deluge";    cond = config.services.deluge.enable; }
    { name = "syncthing"; cond = config.services.syncthing.enable; }
    { name = "adguard";   cond = config.services.adguardhome.enable; }
    { name = "sonarr";    cond = config.services.sonarr.enable; }
    { name = "bazarr";    cond = config.services.bazarr.enable; }
    { name = "radarr";    cond = config.services.radarr.enable; }
    { name = "prowlarr";  cond = config.services.prowlarr.enable; }
    { name = "jackett";   cond = config.services.jackett.enable; }
    { name = "jellyfin";  cond = config.services.jellyfin.enable; }
    { name = "calibre-web"; cond = config.services.calibre-web.enable; }
    { name = "homepage";  cond = config.services.homepage-dashboard.enable; }
    { name = "prometheus"; cond = config.services.prometheus.enable; }
    { name = "sabnzbd";   cond = config.services.sabnzbd.enable; }
    { name = "grafana";   cond = config.services.grafana.enable; }
  ];

  # lib.concatMap iterates over rewriteEntries, passing each element as `entry`
  # to the lambda. lib.optional returns [ attrset ] when entry.cond is true,
  # or [] when false. concatMap then flattens all the results into one list.
  mkRewrites = lib.concatMap (entry:
    lib.optional entry.cond {
      domain = "${entry.name}.${hostSpecific.hostName}";
      answer = hostIP;
      enabled = true;
    }
  ) rewriteEntries;
in
{

  config = lib.mkIf config.services.adguardhome.enable {
    # For dns port(53) accessed port need to be opend.
    networking.firewall.allowedTCPPorts = [ dnsPort ];
    networking.firewall.allowedUDPPorts = [ dnsPort ];

    # file generated at /var/lib/private/AdGuardHome/AdGuardHome.yaml
    services.adguardhome = {
      mutableSettings = false;
      openFirewall = true; # Opens the firewall for AdGuard (optional)
      port = 3000;

      # Basic settings
      settings = {
        dns = {
          port = dnsPort; # DNS port for AdGuard
          upstream_dns = [
            "8.8.8.8"
            "8.8.4.4"
          ]; # Upstream DNS servers (Google DNS in this case)
          bootstrap_dns = [
            "8.8.8.8"
            "8.8.4.4"
          ]; # Upstream DNS servers (Google DNS in this case)

        };

        filtering = {
          protection_enabled = true;
          filtering_enabled = true;
          rewrites = mkRewrites;
        };
      };
    };
  };
}
