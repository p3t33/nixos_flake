# AdGuard Home is disabled in favor of Unbound, but retained as a fallback DNS
# service for networks where the router cannot provide local DNS overrides.
# Upstream resolution uses DNS over TLS.
{ config, lib, hostSpecific, ... }:
let
  dnsPort = 53;
  dnsHostIP = config.custom.shared.lan.hosts.${hostSpecific.hostName}.ipv4Address;
  appsProxyIP = config.custom.shared.lan.hosts.nas.ipv4Address;
  appsDomain = config.custom.shared.appsDomain;

  # IMPORTANT: This fallback DNS routing has not been runtime-tested. Before
  # directing clients to AdGuard, verify every rewrite, canonical HTTPS route,
  # upstream DNS-over-TLS path, and client DNS configuration.
  applicationNames = [
    "calibre-web"
    "deluge"
    "gatus"
    "grafana"
    "home-assistant"
    "homepage"
    "immich"
    "jellyfin"
    "n8n"
    "paperless"
    "prometheus"
    "prowlarr"
    "qbittorrent"
    "radarr"
    "sabnzbd"
    "sonarr"
    "syncthing"
    "zigbee2mqtt"
  ];

  rewriteEntries = map (name: {
    domain = "${name}.${appsDomain}";
    answer = appsProxyIP;
    enabled = true;
  }) applicationNames ++ [{
    domain = "adguard.${appsDomain}";
    answer = dnsHostIP;
    enabled = true;
  }];
in
{

  config = lib.mkIf config.services.adguardhome.enable {
    # For dns port(53) accessed port need to be opend.
    networking.firewall.allowedTCPPorts = [ dnsPort ];
    networking.firewall.allowedUDPPorts = [ dnsPort ];

    systemd.services.adguardhome = {
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
    };

    # file generated at /var/lib/private/AdGuardHome/AdGuardHome.yaml
    services.adguardhome = {
      mutableSettings = false;
      openFirewall = true; # Opens the firewall for AdGuard (optional)
      port = 3000;

      # Basic settings
      settings = {
        dns = {
          bind_hosts = [
            "127.0.0.1"
            dnsHostIP
          ];
          port = dnsPort; # DNS port for AdGuard
          upstream_dns = [
            "tls://one.one.one.one"
          ];
          fallback_dns = [
            "tls://8.8.8.8"
          ];
          bootstrap_dns = [
            "1.1.1.1"
            "8.8.8.8"
          ];

        };

        filtering = {
          protection_enabled = true;
          filtering_enabled = true;
          rewrites = rewriteEntries;
        };
      };
    };
  };
}
