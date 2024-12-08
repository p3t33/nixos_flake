{ config, ... }:
{
  # /etc/homepage-dashboard/services.yaml
  services.homepage-dashboard = {
    enable = true;                     # Enable the service
    listenPort = config.userDefinedGlobalVariables.servicePort.homepageDashboard;
    openFirewall = true;               # Open the firewall for the port
    widgets = [

    {
    resources = {
      cpu = true;
      disk = "/";
      memory = true;
    };
  }

    ];
    services = [
  {
    "media" = [
      {
        "deluge" = {
          description = "Deluge Web UI";
          href = "http://10.100.102.73/deluge"; # URL to the Deluge service
          icon = "deluge.png";                  # Icon for Deluge
        };
      }
      {
        "sonarr" = {
          description = "Deluge Web UI";
          href = "http://10.100.102.73/sonarr"; # URL to the Deluge service
          icon = "sonarr.png";                  # Icon for Deluge
        };
      }
      {
        "radarr" = {
          description = "Deluge Web UI";
          href = "http://10.100.102.73/radarr"; # URL to the Deluge service
          icon = "radarr.png";                  # Icon for Deluge
        };
      }
      {
        "prowlarr" = {
          description = "Deluge Web UI";
          href = "http://10.100.102.73/prowlarr"; # URL to the Deluge service
          icon = "prowlarr.png";                  # Icon for Deluge
        };
      }
      {
        "jackett" = {
          description = "Deluge Web UI";
          href = "http://10.100.102.73/jackett"; # URL to the Deluge service
          icon = "jackett.png";                  # Icon for Deluge
        };
      }
      {
        "jellyfin" = {
          description = "Deluge Web UI";
          href = "http://10.100.102.73/jellyfin"; # URL to the Deluge service
          icon = "jellyfin.png";                  # Icon for Deluge
        };
      }
    ];
  }
  {
    "files" = [
      {
        "syncthing" = {
          description = "Syncthing Web GUI";
          href = "http://10.100.102.73/syncthing"; # URL to the Syncthing service
          icon = "syncthing.png";                 # Icon for Syncthing
        };
      }
    ];
  }
  {
    "filtering" = [
      {
        "adguard" = {
          description = "Syncthing Web GUI";
          href = "http://10.100.102.73/adguard"; # URL to the Syncthing service
          icon = "adguard-home.png";                 # Icon for Syncthing
        };
      }
    ];
  }
];
    settings = {
      # base = "http://homepage.homelab";
      title = "My Dashboard";          # Set the title of your homepage
    };
  };
}
