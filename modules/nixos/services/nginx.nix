{ ... }:
{
  services.nginx = {
    enable = true;
    statusPage = true;

    # Sets proxy_set_headers such as "Host $host;" and gets included
    # by the locations.<name> as a file and in this way eliminates some
    # code repeat.
    recommendedProxySettings = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "10.100.102.73" = {
        locations."/deluge" = {
          proxyPass = "http://localhost:8112/"; # Reverse proxy to Deluge Web UI
          extraConfig = ''
            proxy_set_header X-Deluge-Base "/deluge/";
            add_header X-Frame-Options SAMEORIGIN;
          '';
        };

        locations."/adguard/" = {
          proxyPass = "http://localhost:3000/"; # Reverse proxy to Deluge Web UI
        };

        # This will not work by itself and will return "Host check error"
        # due to security reasons. The recommendation was to set:
        # gui.insecureSkipHostcheck in the advanced settings.
        #
        # As my current syncthing settings are not fully deliberative this needs
        # to be set by hand on every fresh install of syncthing NixOS module that
        # is intended to work behind a reverse proxy such as nginx.
        #
        # Note setting proxy_set_header settings in the extraConfig might conflict with
        # the once that are included in the file generated by recommendedProxySettings.
        # this will cause "400 Bad Request"
        locations."/syncthing/" = {
          proxyPass = "http://localhost:8384/"; # Reverse proxy to syncthing Web UI
          extraConfig = ''
            proxy_read_timeout 600s;
            proxy_send_timeout 600s;
          '';
        };
      };
    };

  };

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
}
