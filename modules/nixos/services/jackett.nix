{ config, pkgs, ... }:

{

  # Optional: Network firewall settings (only if Prowlarr needs to be accessed remotely)
  #networking.firewall.allowedTCPPorts = [ 9171 ]; # Opens port 9696, Prowlarr's default port

  services.jackett = {
    enable = true;                 # Starts the Jackett service
    openFirewall = true;           # Opens firewall for external access (optional) # 9117
    port = config.userDefinedGlobalVariables.servicePort.jackett;
  };

  # Optional: Open firewall for Jackett's default port (9117) if needed
  # Optional: Prometheus Exportarr Configuration (for monitoring with Prometheus)
  # services.prometheus.exporters.exportarr-prowlarr = {
  #   enable = false;              # Enable this only if you want to monitor Prowlarr with Prometheus
  #   port = 9100;                 # Port for Prometheus metrics (customize as needed)
  #   apiKeyFile = "/path/to/prowlarr-api-key"; # Path to file with the API key for secure access (optional)
  # };
}
