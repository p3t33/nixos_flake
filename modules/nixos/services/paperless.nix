{ config, lib, pkgs, hostSpecific, ... }:
let
  paperlessPath = "${config.custom.shared.pathToMediaDirectory}/paperless";
  appsDomain = config.custom.shared.appsDomain;
  paperlessHost = "paperless.${appsDomain}";
  externalScheme = if config.custom.security.acme.enable then "https" else "http";
in
{
  config = lib.mkIf config.services.paperless.enable {
    sops.secrets.paperless-ngx-env = {};

    # There is also services.paperless-ngx but the module to
    # use is this one with the default pacakge being paperless-ngx
    services.paperless = {
      # This is the default package, I state this for readability.
      package = pkgs.paperless-ngx;
      address = config.custom.shared.localHostIPv4;
      port = 28981;
      database.createLocally = true;
      environmentFile = config.sops.secrets.paperless-ngx-env.path;

      # Storage: Splitting app logic from bulk media
      dataDir = "/var/lib/paperless";        # Search index (fast SSD)
      mediaDir = "${paperlessPath}/media";   # Documents (bulk HDD)
      consumptionDir = "${paperlessPath}/consume";

      settings = {
        PAPERLESS_TIME_ZONE = "Asia/Jerusalem";
        PAPERLESS_OCR_LANGUAGE = "heb+eng";  # Hebrew + English OCR
        PAPERLESS_OCR_MODE = "skip"; # Don't re-OCR if text exists
        PAPERLESS_OCR_IMAGE_DPI = "300";
        PAPERLESS_FILENAME_FORMAT = "{{ correspondent }}/{{ created_year }}/{{ title }}";
        PAPERLESS_URL = "${externalScheme}://${paperlessHost}";
        PAPERLESS_ALLOWED_HOSTS = "${paperlessHost},${config.custom.shared.lan.hosts.${hostSpecific.hostName}.ipv4Address},${config.custom.shared.localHostIPv4}";
        PAPERLESS_USE_X_FORWARD_HOST = true;
        PAPERLESS_USE_X_FORWARD_PORT = true;
        PAPERLESS_PROXY_SSL_HEADER = [ "HTTP_X_FORWARDED_PROTO" "https" ];
      };

      # The "Portable" Daily Backup
      exporter = {
        enable = true;
        directory = "${paperlessPath}/export";
        onCalendar = "02:00";
        settings = {
          compare-checksums = true;
          delete = true;
          no-progress-bar = true;
          no-color = true;
        };
      };
    };

    systemd.tmpfiles.rules = [
      "d ${paperlessPath} 0770 ${config.services.paperless.user} ${config.custom.shared.mediaGroup} -"
      "d ${config.services.paperless.mediaDir} 0770 ${config.services.paperless.user} ${config.custom.shared.mediaGroup} -"
      "d ${config.services.paperless.consumptionDir} 0770 ${config.services.paperless.user} ${config.custom.shared.mediaGroup} -"
      "d ${config.services.paperless.exporter.directory} 0770 ${config.services.paperless.user} ${config.custom.shared.mediaGroup} -"
    ];
  };
}
