{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.services.qbittorrent;
  categoryNames = config.custom.media.downloadCategories;
  mediaDir = config.custom.shared.pathToMediaDirectory;
  torrentsDir = "${mediaDir}/torrents";
  configPath = "${cfg.profileDir}/qBittorrent/config";
  configFilePath = "${configPath}/qBittorrent.conf";
  categoriesPath = "${configPath}/categories.json";
  qBittorrentServerConfig = {
    BitTorrent = {
      "Session\\DefaultSavePath" = cfg.categories.${categoryNames.default} or cfg.downloadDir;
      "Session\\DHTEnabled" = false;
      "Session\\DisableAutoTMMByDefault" = false;
      "Session\\LSDEnabled" = false;
      "Session\\PeXEnabled" = false;
      "Session\\QueueingSystemEnabled" = false;
      "Session\\TempPath" = cfg.incompleteDir;
      "Session\\TempPathEnabled" = true;
    };

    LegalNotice.Accepted = true;

    Network.PortForwardingEnabled = false;
  };
  qBittorrentConfigFile = pkgs.writeText "qBittorrent.conf" (
    lib.generators.toINI { } qBittorrentServerConfig
  );
  categoriesFile = pkgs.writeText "qbittorrent-categories.json" (
    builtins.toJSON (lib.mapAttrs (_name: path: { save_path = path; }) cfg.categories)
  );
  mergeCategoriesScript = pkgs.writeShellScript "qbittorrent-merge-categories" ''
    set -euo pipefail

    categories_path=${lib.escapeShellArg categoriesPath}
    declared_categories=${lib.escapeShellArg categoriesFile}
    existing_categories=$(${lib.getExe' pkgs.coreutils "mktemp"})
    merged_categories=$(${lib.getExe' pkgs.coreutils "mktemp"})

    cleanup() {
      rm -f "$existing_categories" "$merged_categories"
    }
    trap cleanup EXIT

    if [ -s "$categories_path" ] && ${lib.getExe pkgs.jq} -e 'type == "object"' "$categories_path" >/dev/null 2>&1; then
      ${lib.getExe' pkgs.coreutils "cp"} "$categories_path" "$existing_categories"
    else
      printf '{}' > "$existing_categories"
    fi

    ${lib.getExe pkgs.jq} -s '.[0] * .[1]' "$existing_categories" "$declared_categories" > "$merged_categories"
    ${lib.getExe' pkgs.coreutils "chmod"} 640 "$merged_categories"
    ${lib.getExe' pkgs.coreutils "mv"} "$merged_categories" "$categories_path"
  '';
  preStartScript = pkgs.writeShellScript "qbittorrent-pre-start" ''
    set -euo pipefail

    ${lib.getExe' pkgs.coreutils "install"} -Dm600 ${lib.escapeShellArg qBittorrentConfigFile} ${lib.escapeShellArg configFilePath}

    {
      printf '\n[Preferences]\n'
      printf 'WebUI\\Username='
      ${lib.getExe' pkgs.coreutils "cat"} ${lib.escapeShellArg config.sops.secrets."qbittorrent/webui_username".path}
      printf '\nWebUI\\Password_PBKDF2='
      ${lib.getExe' pkgs.coreutils "cat"} ${lib.escapeShellArg config.sops.secrets."qbittorrent/webui_password_pbkdf2".path}
      printf '\n'
    } >> ${lib.escapeShellArg configFilePath}

    ${mergeCategoriesScript}
  '';
in
{
  options.custom.services.qbittorrent = {
    enable = lib.mkEnableOption "qBittorrent headless service";

    profileDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/qBittorrent";
      description = "Directory passed to qBittorrent via --profile.";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "qbittorrent";
      description = "User account under which qBittorrent runs.";
    };

    webuiPort = lib.mkOption {
      type = lib.types.port;
      default = 8090;
      description = "qBittorrent web UI port.";
    };

    torrentingPort = lib.mkOption {
      type = lib.types.port;
      default = 6630;
      description = "qBittorrent incoming torrent port.";
    };

    downloadDir = lib.mkOption {
      type = lib.types.str;
      default = "${torrentsDir}/qbittorrent";
      description = "Base directory for qBittorrent downloads.";
    };

    incompleteDir = lib.mkOption {
      type = lib.types.str;
      default = "${cfg.downloadDir}/incomplete";
      description = "Directory for incomplete qBittorrent downloads.";
    };

    categories = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {
        ${categoryNames.default} = "${cfg.downloadDir}/${categoryNames.default}";
        ${categoryNames.movies} = "${cfg.downloadDir}/${categoryNames.movies}";
        ${categoryNames.tv} = "${cfg.downloadDir}/${categoryNames.tv}";
        ${categoryNames.ebooks} = "${cfg.downloadDir}/${categoryNames.ebooks}";
        ${categoryNames.audiobooks} = "${cfg.downloadDir}/${categoryNames.audiobooks}";
      };
      description = "Map of qBittorrent category names to save paths.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.qbittorrent = {
      enable = true;
      inherit (cfg) user profileDir webuiPort torrentingPort;
      group = config.custom.shared.mediaGroup;
      openFirewall = true;
      extraArgs = [ "--confirm-legal-notice" ];
      serverConfig = qBittorrentServerConfig;
    };

    # The upstream qBittorrent openFirewall option exposes the Web UI and
    # torrenting TCP ports. Torrent peer traffic also uses UDP, which the
    # upstream option does not open.
    networking.firewall.allowedUDPPorts = [ cfg.torrentingPort ];

    sops.secrets."qbittorrent/webui_username" = {
      owner = config.services.qbittorrent.user;
      mode = "0400";
      restartUnits = [ config.systemd.services.qbittorrent.name ];
    };

    sops.secrets."qbittorrent/webui_password" = { };

    sops.secrets."qbittorrent/webui_password_pbkdf2" = {
      owner = config.services.qbittorrent.user;
      mode = "0400";
      restartUnits = [ config.systemd.services.qbittorrent.name ];
    };

    systemd.tmpfiles.rules =
      lib.optional (!config.services.deluge.enable)
        "d ${torrentsDir} 2770 root ${config.custom.shared.mediaGroup} -"
      ++ [
        "d ${cfg.downloadDir} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
        "d ${cfg.incompleteDir} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
      ]
      ++ lib.mapAttrsToList (
        _name: path: "d ${path} 2770 ${cfg.user} ${config.custom.shared.mediaGroup} -"
      ) cfg.categories;

    systemd.services.qbittorrent.serviceConfig.ExecStartPre = lib.mkForce [
      preStartScript
    ];
  };
}
