{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.waybar;
  colors = config.custom.shared.colors;
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
        position = "bottom";
        height = 38;
        spacing = 0;

        modules-left = [
          "sway/workspaces"
          "sway/mode"
        ];
        modules-right = [
          "tray"
          "disk"
          "memory"
          "cpu"
        ]
        ++ lib.optionals config.custom.waybar.enableWlan [ "network" ]
        ++ lib.optionals config.custom.waybar.enableBattery [ "custom/battery" ]
        ++ [
          "pulseaudio"
          "clock"
        ]
        ++ lib.optionals config.custom.waybar.enableAllenTxTime [ "custom/allen_tx" ];

        "sway/workspaces" = {
          all-outputs = false;
          reverse-scroll = true;
          format = "{value}";
        };

        "sway/mode" = {
          format = "{}";
        };

        disk = {
          interval = 25;
          path = "/";
          format = "<span foreground='${colors.primary}'>󱛟 DISK:</span> {percentage_used:2}%";
        };

        memory = {
          interval = 2;
          format = "<span foreground='${colors.primary}'>󰍛 RAM:</span> {percentage:2}%";
        };

        cpu = {
          interval = 2;
          format = "<span foreground='${colors.primary}'>󰻠 CPU:</span> {usage:2}%";
        };

        network = {
          interface = "wl*";
          interval = 5;
          format-wifi = "<span foreground='${colors.primary}'>{icon}</span> {signalStrength}%";
          format-disconnected = "<span foreground='${colors.disabled}'>󰖪</span>";
          format-disabled = "<span foreground='${colors.disabled}'>󰖪</span>";
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
          format = "<span foreground='${colors.primary}'>{icon}</span> {percentage:3}%";
          format-icons = [
            "󰁺"
            "󰁽"
            "󰁾"
            "󰂀"
            "󰁹"
          ];
        };

        pulseaudio = {
          format = "<span foreground='${colors.primary}'>{icon}</span> {volume}%";
          format-muted = "<span foreground='${colors.primary}'>󰝟</span>";
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
          exec = "TZ=America/Chicago ${date} +\"US: %H:%M\"";
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
          color: ${colors.foreground};
        }

        #workspaces button.focused {
          background: ${colors.background-alt};
          color: ${colors.primary};
          box-shadow: inset 0 -2px ${colors.primary};
        }

        #workspaces button.urgent {
          color: ${colors.alert};
        }

        #workspaces button:hover {
          background: ${colors.background-alt};
          box-shadow: inherit;
          text-shadow: inherit;
        }

        #mode {
          padding: 0 16px;
          background: ${colors.background-alt};
          box-shadow: inset 0 -2px ${colors.primary};
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
          background: ${colors.background-alt};
        }
      '';
    };
  };
}
