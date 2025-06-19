{ config, lib, ... }:

let
  serviceName = "prowlarr";
  cfg = config.customOptions.enableModule.${serviceName};
in
{
    options.customOptions.enableModule.${serviceName} = lib.mkEnableOption "Enable the Prowlarr indexer management service";

    config = lib.mkIf cfg {
    sops.secrets."${serviceName}/apiKey" = {};

    services.${serviceName} = {
      enable = true;

      openFirewall = true;
      settings = {
        server = {
          port = 9696;
          urlbase = "/${serviceName}";
        };
      };

      environmentFiles = [
        config.sops.secrets."${serviceName}/apiKey".path
      ];

    };
  };
}
