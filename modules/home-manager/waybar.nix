{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.waybar;
  workspaceAccent = config.custom.shared.outlineColors.active;
  colors = config.custom.shared.omarchyColors // {
    background = "#2e3440";
    foreground = "#d8dee9";
    bright_foreground = "#d8dee9";
    dark_foreground = "#4c566a";
    selection = "#3b4252";
    lighter_background = "#3b4252";
    red = "#bf616a";
  };
  date = lib.getExe' pkgs.coreutils "date";
  batteryStatus = pkgs.writeShellScript "waybar-battery-status" ''
    capacity_file=/sys/class/power_supply/BAT0/capacity

    if ! IFS= read -r capacity < "$capacity_file" || [[ ! $capacity =~ ^[0-9]+$ ]]; then
      printf '{"text":"","percentage":0}\n'
      exit 0
    fi

    if (( capacity >= 95 )); then
      capacity=100
    fi

    printf '{"text":"available","percentage":%d}\n' "$capacity"
  '';
in
{
  options.custom.waybar = {
    enableWlan = lib.mkEnableOption "Wi-Fi status in Waybar";
    enableBattery = lib.mkEnableOption "battery status in Waybar";
    enableAllenTxTime = lib.mkEnableOption "Allen, TX time in Waybar";
  };

  config = lib.mkIf cfg.enable {
    xsession.preferStatusNotifierItems = true;

    programs.waybar = {
      systemd.enable = true;

      settings.mainBar = {
        layer = "top";
        position = "top";
        height = 38;
        spacing = 0;

        modules-left = [
          "sway/workspaces"
          "sway/mode"
        ];
        modules-center = [ "clock" ]
          ++ lib.optionals config.custom.waybar.enableAllenTxTime [ "custom/allen_tx" ];

        modules-right = [
          "tray"
          "disk"
          "memory"
          "cpu"
        ]
        ++ lib.optionals config.custom.waybar.enableWlan [ "network" ]
        ++ lib.optionals config.custom.waybar.enableBattery [ "custom/battery" ]
        ++ [ "pulseaudio" ];

        "sway/workspaces" = {
          all-outputs = false;
          reverse-scroll = true;
          format = "{index}";
        };

        "sway/mode" = {
          format = "{}";
        };

        disk = {
          interval = 25;
          path = "/";
          format = "<span foreground='${colors.foreground}'>󱛟 DISK:</span> {percentage_used:2}%";
        };

        memory = {
          interval = 2;
          format = "<span foreground='${colors.foreground}'>󰍛 RAM:</span> {percentage:2}%";
        };

        cpu = {
          interval = 2;
          format = "<span foreground='${colors.foreground}'>󰻠 CPU:</span> {usage:2}%";
        };

        network = {
          interface = "wl*";
          interval = 5;
          format-wifi = "<span foreground='${colors.foreground}'>{icon}</span> {signalStrength}%";
          format-disconnected = "<span foreground='${colors.dark_foreground}'>󰖪</span>";
          format-disabled = "<span foreground='${colors.dark_foreground}'>󰖪</span>";
          format-icons = [
            "󰤟"
            "󰤟"
            "󰤢"
            "󰤥"
            "󰤨"
          ];
        };

        "custom/battery" = {
          exec = batteryStatus;
          interval = 5;
          return-type = "json";
          hide-empty-text = true;
          format = "<span foreground='${colors.foreground}'>{icon}</span> {percentage:3}%";
          format-icons = [
            "󰁺"
            "󰁽"
            "󰁾"
            "󰂀"
            "󰁹"
          ];
        };

        pulseaudio = {
          format = "<span foreground='${colors.foreground}'>{icon}</span> {volume}%";
          format-muted = "<span foreground='${colors.foreground}'>󰝟</span>";
          format-icons.default = [
            "󰕿"
            "󰖀"
            "󰕾"
          ];
        };

        clock = {
          interval = 1;
          format = " {:%d-%m-%Y  %H:%M}";
        };

        "custom/allen_tx" = {
          exec = "TZ=America/Chicago ${date} +\"US: %d-%m-%Y %H:%M\"";
          interval = 30;
          format = "{}";
          tooltip = false;
        };

        tray = {
          icon-size = 20;
          spacing = 8;
        };
      };

      style = ''
        * {
          border: none;
          border-radius: 0;
          font-family: "${config.custom.shared.font.mono}";
          font-size: 15px;
          min-height: 0;
        }

        window#waybar {
          background: ${colors.background};
          color: ${colors.foreground};
        }

        #workspaces button {
          padding: 0 16px;
          background: transparent;
          color: ${colors.bright_foreground};
        }

        #workspaces button.focused {
          background: mix(${colors.background}, ${workspaceAccent}, 0.12);
          color: ${colors.bright_foreground};
          box-shadow: inset 0 0 6px alpha(${workspaceAccent}, 0.45);
        }

        #workspaces button.urgent {
          color: ${colors.red};
        }

        #workspaces button:hover:not(.focused) {
          background: ${colors.lighter_background};
          box-shadow: inherit;
          text-shadow: inherit;
        }

        #mode {
          padding: 0 16px;
          background: ${colors.selection};
          box-shadow: inset 0 -2px ${colors.accent};
        }

        #tray {
          padding: 0 12px;
        }

        #disk,
        #memory,
        #cpu,
        #network,
        #custom-battery,
        #pulseaudio {
          padding: 0 8px;
        }

        #clock,
        #custom-allen_tx {
          padding: 0 16px;
          background: transparent;
        }
      '';
    };
  };
}
