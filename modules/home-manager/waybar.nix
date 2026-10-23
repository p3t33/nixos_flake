{
  config,
  lib,
  ...
}:

let
  cfg = config.programs.waybar;
  workspaceAccent = config.custom.shared.outlineColors.active;
  colors = config.custom.shared.omarchyColors // {
    background = "#3f4545";
    foreground = "#ffffff";
    bright_foreground = "#ffffff";
    dark_foreground = "#a5abb6";
    selection = "#3b4252";
    lighter_background = "#4c566a";
    workspace_background = "#363a42";
    red = "#bf616a";
  };
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
        modules-center = [
          "clock"
        ]
        ++ lib.optionals config.custom.waybar.enableAllenTxTime [ "clock#allen_tx" ];

        modules-right = [
          "privacy"
          "sway/language"
          "disk"
          "memory"
          "cpu"
        ]
        ++ lib.optionals config.custom.waybar.enableWlan [ "network" ]
        ++ lib.optionals config.custom.waybar.enableBattery [ "battery" ]
        ++ [ "pulseaudio" ];

        "sway/workspaces" = {
          all-outputs = false;
          reverse-scroll = true;
          format = "{index}";
        };

        "sway/mode" = {
          format = "{}";
        };

        "sway/language" = {
          format = "{flag}";
          tooltip-format = "{long}";
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

        battery =
          let
            actualCapacityFormat = "{icon} {capacity:3}% {time}";
          in
          {
            bat = "BAT0";
            interval = 5;
            states = {
              below-full = 94;
              warning = 30;
              critical = 15;
            };
            format = "{icon} 100% {time}";
            format-below-full = actualCapacityFormat;
            format-warning = actualCapacityFormat;
            format-critical = actualCapacityFormat;
            format-time = "{H}h{m}m";
            format-icons = {
              default = [
                "󰁺"
                "󰁽"
                "󰁾"
                "󰂀"
                "󰁹"
              ];
              charging = "󰂄";
              plugged = "󰚥";
              full = "󰁹";
            };
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

        "clock#allen_tx" = {
          timezone = "America/Chicago";
          interval = 30;
          format = "US: {:%d-%m-%Y %H:%M}";
          tooltip = false;
        };

        privacy = {
          icon-size = 20;
          icon-spacing = 8;
          modules = [
            { type = "audio-in"; }
            { type = "screenshare"; }
          ];
        };

      };

      style = ''
        * {
          border: none;
          border-radius: 0;
          font-family: "${config.custom.shared.font.mono}";
          font-size: 17px;
          font-weight: 700;
          min-height: 0;
        }

        window#waybar {
          background: ${colors.background};
          color: ${colors.foreground};
        }

        #workspaces button {
          margin: 5px 0;
          padding: 0 10px;
          border-radius: 4px;
          background: transparent;
          color: ${colors.bright_foreground};
        }

        #workspaces button.focused {
          background: ${workspaceAccent};
          color: ${colors.background};
          box-shadow: none;
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

        #language,
        #disk,
        #memory,
        #cpu,
        #network,
        #battery,
        #pulseaudio {
          padding: 0 8px;
        }

        #battery.discharging.warning {
          color: ${colors.yellow};
        }

        #battery.discharging.critical {
          color: ${colors.red};
        }

        #privacy-item {
          padding: 0 8px;
          color: ${colors.red};
        }

        #clock {
          padding: 0 16px;
          background: transparent;
          color: ${colors.foreground};
          font-size: 18px;
          font-weight: 700;
        }
      '';
    };
  };
}
