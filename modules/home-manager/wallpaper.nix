{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.custom.desktop.wallpaper;
  swayEnabled = config.wayland.windowManager.sway.enable;
  rwpspreadCache = "${config.xdg.cacheHome}/rwpspread";

  restartSwaybg = pkgs.writeShellScript "restart-rwpspread-swaybg" ''
    exec ${lib.getExe' pkgs.systemd "systemctl"} --user restart rwpspread-background.service
  '';

  runSwaybg = pkgs.writeShellScript "run-rwpspread-swaybg" ''
    shopt -s nullglob

    wallpapers=()
    for wallpaper in ${lib.escapeShellArg rwpspreadCache}/rwps_*.png; do
      [ -L "$wallpaper" ] || continue
      output=''${wallpaper##*/rwps_}
      output=''${output%.png}
      wallpapers+=(--output "$output" --image "$wallpaper")
    done

    if [ "''${#wallpapers[@]}" -eq 0 ]; then
      echo "No generated wallpapers found in ${rwpspreadCache}." >&2
      exit 1
    fi

    exec ${lib.getExe pkgs.swaybg} "''${wallpapers[@]}"
  '';
in
{
  options.custom.desktop.wallpaper = {
    enable = lib.mkEnableOption "Enable wallpaper setup";

    name = lib.mkOption {
      type = lib.types.str;
      default = "watchtower.png";
      description = "The name of the wallpaper file";
    };

    pathOut = lib.mkOption {
      type = lib.types.str;
      default = ""; # will be set below in config using mkDefault
      description = "The path where the wallpaper will be located inside the XDG config";
    };

    pathIn = lib.mkOption {
      type = lib.types.path;
      default = ../../wallpaper/watchtower.png; # will be overridden below based on wallpaperName
      description = "The source path to the wallpaper inside the repository";
    };

  };

  config = lib.mkIf cfg.enable {
    custom.desktop.wallpaper.pathOut = lib.mkDefault "wallpaper/${cfg.name}";
    custom.desktop.wallpaper.pathIn = lib.mkDefault ../../wallpaper/${cfg.name};

    xdg.configFile."${cfg.pathOut}".source = cfg.pathIn;

    home.packages = lib.optionals swayEnabled [ pkgs.rwpspread ];

    systemd.user.services = lib.mkIf swayEnabled {
      rwpspread = {
        Unit = {
          Description = "Generate wallpaper slices for Wayland outputs";
          After = [ "sway-session.target" ];
          PartOf = [ "sway-session.target" ];
          ConditionEnvironment = "WAYLAND_DISPLAY";
        };

        Service = {
          Type = "simple";
          ExecStart = "${lib.getExe pkgs.rwpspread} --daemon --image ${cfg.pathIn} --post ${restartSwaybg}";
          Restart = "on-failure";
          RestartSec = 1;
        };

        Install.WantedBy = [ "sway-session.target" ];
      };

      rwpspread-background = {
        Unit = {
          Description = "Display wallpaper slices across Wayland outputs";
          After = [ "rwpspread.service" ];
          PartOf = [ "sway-session.target" ];
          ConditionEnvironment = "WAYLAND_DISPLAY";
        };

        Service = {
          Type = "simple";
          ExecStart = runSwaybg;
          Restart = "on-failure";
          RestartSec = 1;
        };
      };
    };
  };
}
