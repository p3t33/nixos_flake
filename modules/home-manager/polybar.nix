{ config, lib, pkgs, ... }:

let
  ws1 = "${config.custom.i3.workspaces.ws1}";
  ws2 = "${config.custom.i3.workspaces.ws2}";
  ws3 = "${config.custom.i3.workspaces.ws3}";
  ws4 = "${config.custom.i3.workspaces.ws4}";
  ws5 = "${config.custom.i3.workspaces.ws5}";
  ws6 = "${config.custom.i3.workspaces.ws6}";
  ws7 = "${config.custom.i3.workspaces.ws7}";
  ws8 = "${config.custom.i3.workspaces.ws8}";
  ws9 = "${config.custom.i3.workspaces.ws9}";
  ws10 ="${config.custom.i3.workspaces.ws10}";

  ws1Icon = "${config.custom.i3.workspacesIcons.firefox}";
  ws2Icon = "${config.custom.i3.workspacesIcons.code}";
  ws3Icon = "${config.custom.i3.workspacesIcons.cherrytree}";
  ws4Icon = "${config.custom.i3.workspacesIcons.chrome}";
  ws5Icon = "${config.custom.i3.workspacesIcons.terminal}";
  ws6Icon = "${config.custom.i3.workspacesIcons.buildserver}";
  ws8Icon = "${config.custom.i3.workspacesIcons.default}";
  defaultIcon = "${config.custom.i3.workspacesIcons.default}";
in
{
  options.custom.polybar = {
    enableWlan = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable the wlan module in polybar.";
    };
    enableBattery = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable the battery module in polybar.";
    };
    enableAllenTxTime = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable the Allen, TX time module in polybar.";
    };
  };

  config = lib.mkIf config.services.polybar.enable {
    services.polybar = {
      package = pkgs.polybar.override {
        i3Support = true;
        alsaSupport = true;
        iwSupport = true;
        githubSupport = true;
      };
      config = {
        "bar/bottom" = {
          monitor = "\${env:MONITOR:}";
          #monitor = "Virtual-1";
          width = "100%";
          height = "38";
          #radius = 0;

          # put the bar at the top
          bottom = true;
          foreground = "${config.custom.shared.colors.foreground}";
          background = "${config.custom.shared.colors.background}";

          # underline / overline
          line-size = 2;
          line-color = "${config.custom.shared.colors.primary}";
          border-size = 0;
          padding = 0;
          module-margin = 1;

          # using patched mono font for the icons might change it later on.
          font-0 = "${config.custom.shared.font.mono}:style=Regular:size=15;4";
          # Just sticking them together in the center for now
          modules-left = "i3";
          modules-right = lib.strings.concatStringsSep " " ([
            "filesystem" "memory" "cpu"
          ]
          ++ (lib.optionals config.custom.polybar.enableWlan [ "wlan" ])
          ++ (lib.optionals config.custom.polybar.enableBattery [ "battery" ])
          ++ [ "volume" "date" ]
          ++ (lib.optionals config.custom.polybar.enableAllenTxTime [ "allen_tx" ]));
          #modules-center = "i3";

          cursor-click = "pointer";
          cursor-scroll = "ns-resize";
          enable-ipc = true;
        };

        "module/allen_tx" = {
          type = "custom/script";
          exec = "TZ=America/Chicago /run/current-system/sw/bin/date +\"US: %H:%M\"";
          interval = 30;
          format-foreground = "${config.custom.shared.colors.foreground}";
          format-background = "${config.custom.shared.colors.background-alt}";
          format-padding = 2;
        };

        "module/oslogo" = {
          type = "custom/text";
          content = " NixOS";
          content-foreground = "${config.custom.shared.colors.foreground}";
          content-background = "${config.custom.shared.colors.background-alt}";
          content-padding = 2;
        };

        "module/xworkspaces" = {

          type = "internal/xworkspaces";
          pin-workspaces = true;
          enable-scroll = false;

          # Use fuzzy (partial) matching on labels when assigning
          # icons to workspaces.
          # Example: code;♚ will apply the icon to all workspaces
          # containing 'code' in the label
          # Default: false
          fuzzy-match = true;

          icon-0 = "${ws10};${defaultIcon}";
          icon-1 = "${ws1};${ws1Icon}";
          icon-2 = "${ws2};${ws2Icon}";
          icon-3 = "${ws3};${ws3Icon}";
          icon-4 = "${ws4};${ws4Icon}";
          icon-5 = "${ws5};${ws5Icon}";
          icon-6 = "${ws6};${ws6Icon}";
          icon-7 = "${ws7};${defaultIcon}";
          icon-8 = "${ws8};${ws8Icon}";
          icon-9 = "${ws9};${defaultIcon}";
          icon-default = "${defaultIcon}";

          format = "<label-state>";

          label-active = "%name%";
          label-active-foreground = "${config.custom.shared.colors.primary}";
          label-active-background = "${config.custom.shared.colors.background-alt}";
          label-active-underline = "${config.custom.shared.colors.primary}";

          label-occupied = "%name%";

          label-urgent = "%icon%";
          label-urgent-foreground = "${config.custom.shared.colors.alert}";

          label-empty = "%icon%";
          label-empty-foreground = "${config.custom.shared.colors.disabled}";

          label-active-padding = 2;
          label-occupied-padding = 2;
          label-urgent-padding = 2;
          label-empty-padding = 2;
        };

        "module/xwindow" = {
          type = "internal/xwindow";
          label = "%title:0:40:...%";
          format = "<label>";
          format-prefix = "  ";
          format-prefix-foreground = "${config.custom.shared.colors.primary}";
          label-empty = "NixOS";
        };

        "module/filesystem" = {
          type = "internal/fs";
          interval = 25;
          mount-0 = "/";
          label-mounted = "%{F${config.custom.shared.colors.primary}}󱛟 DISK:%{F-} %percentage_used:2%%";
          label-unmounted = "%mountpoint% not mounted";
          label-unmounted-foreground = "${config.custom.shared.colors.disabled}";
        };

        "module/memory" = {
          type = "internal/memory";
          interval = 2;
          format-prefix = "󰍛 RAM:";
          format-prefix-foreground = "${config.custom.shared.colors.primary}";
          label = "%percentage_used:2%%";
        };

        "module/cpu" = {
          type = "internal/cpu";
          interval = 2;
          format-prefix = "󰻠 CPU:";
          format-prefix-foreground = "${config.custom.shared.colors.primary}";
          label = "%percentage:2%%";
        };

        "module/wlan" = {
          # general
          type = "internal/network";
          interval = 5;
          interface-type = "wireless";

          # Format when connected
          format-connected = "<ramp-signal> <label-connected>";
          label-connected = "%signal%%"; # This will show signal strength as a percentage

          # Define signal strength icons (can be customized)
          ramp-signal-0 = "󰤟";
          ramp-signal-0-foreground = "${config.custom.shared.colors.primary}";
          ramp-signal-1 = "󰤟";
          ramp-signal-1-foreground = "${config.custom.shared.colors.primary}";
          ramp-signal-2 = "󰤢";
          ramp-signal-2-foreground = "${config.custom.shared.colors.primary}";
          ramp-signal-3 = "󰤥";
          ramp-signal-3-foreground = "${config.custom.shared.colors.primary}";
          ramp-signal-4 = "󰤨";
          ramp-signal-4-foreground = "${config.custom.shared.colors.primary}";

          format-disconnected = "<label-disconnected>";
          label-disconnected = "󰖪";
          format-disconnected-foreground = "${config.custom.shared.colors.disabled}";
        };

        "module/date" = {
          type = "internal/date";
          interval = 1;
          date = "%d-%m-%Y  %H:%M";
          label = "%date%";
          format = "<label>";
          format-prefix = " ";
          format-foreground = "${config.custom.shared.colors.foreground}";
          format-background = "${config.custom.shared.colors.background-alt}";
          format-padding = 2;
        };
        "module/volume" = {
          type = "internal/alsa";

          #Volume display settings
          format-volume = "<ramp-volume> <label-volume>";
          format-muted = "<label-muted>";
          label-volume = "%percentage%%";
          label-muted = "󰝟";
          ramp-volume-0 = "󰕿"; # Icon for low volume
          ramp-volume-1 = "󰖀"; # Icon for medium volume
          ramp-volume-2 = "󰕾"; # Icon for high volume

          #Color customization (optional)
          label-volume-foreground = "${config.custom.shared.colors.foreground}";

          label-muted-foreground = "${config.custom.shared.colors.primary}";
          ramp-volume-0-foreground = "${config.custom.shared.colors.primary}";
          ramp-volume-1-foreground = "${config.custom.shared.colors.primary}";
          ramp-volume-2-foreground = "${config.custom.shared.colors.primary}";
        };

        # uses the upower service
        "module/battery" = {
          type = "internal/battery";
          # ls -1 /sys/class/power_supply/
          battery = "BAT0";
          adapter = "AC";

          full-at = 95;

          ramp-capacity-0 = "󰁺";
          ramp-capacity-1 = "󰁽";
          ramp-capacity-2 = "󰁾";
          ramp-capacity-3 = "󰂀";
          ramp-capacity-4 = "󰁹";

          # No specific charging icons, use the same as discharging
          ramp-capacity-0-foreground = "${config.custom.shared.colors.primary}";
          ramp-capacity-1-foreground = "${config.custom.shared.colors.primary}";
          ramp-capacity-2-foreground = "${config.custom.shared.colors.primary}";
          ramp-capacity-3-foreground = "${config.custom.shared.colors.primary}";
          ramp-capacity-4-foreground = "${config.custom.shared.colors.primary}";

          label-charging = "%percentage:3%%";
          label-discharging = "%percentage:3%%";
          label-full = "%percentage:3%%";

          format-charging = "<ramp-capacity> <label-charging>";
          format-discharging = "<ramp-capacity> <label-discharging>";
          format-full = "<ramp-capacity> <label-full>";
          format-charging-prefix-foreground = "${config.custom.shared.colors.primary}";
          format-discharging-prefix-foreground = "${config.custom.shared.colors.primary}";
          format-full-prefix-foreground = "${config.custom.shared.colors.primary}";
      };


        "module/i3" = {
          type = "internal/i3";

          # show monitor specific workspaces
          pin-workspaces = true;

          # this will drop the number from workspace
          # E.g: 1: Firefox --> Firefox
          strip-wsnumbers = true;
          scroll-up = "i3wm-wsnext";
          scroll-down = "i3wm-wsprev";

          # ws-icon-[0-9]+ = <label>;<icon>
          # NOTE: The <label> needs to match the name of the i3 workspace
          ws-icon-0 = "${ws10};${defaultIcon}";
          ws-icon-1 = "${ws1};${ws1Icon}";
          ws-icon-2 = "${ws2};${ws2Icon}";
          ws-icon-3 = "${ws3};${ws3Icon}";
          ws-icon-4 = "${ws4};${ws4Icon}";
          ws-icon-5 = "${ws5};${ws5Icon}";
          ws-icon-6 = "${ws6};${ws6Icon}";
          ws-icon-7 = "${ws7};${defaultIcon}";
          ws-icon-8 = "${ws8};${ws8Icon}";
          ws-icon-9 = "${ws9};${defaultIcon}";
          ws-icon-default = "${defaultIcon}";

          format = "<label-state> <label-mode>";
          label-dimmed-underline = "${config.custom.shared.colors.background}";

          label-mode = "%mode%";
          label-mode-padding = 2;

          label-focused = "%name%";
          label-focused-foreground = "${config.custom.shared.colors.primary}";
          label-focused-background = "${config.custom.shared.colors.background-alt}";
          label-focused-underline = "${config.custom.shared.colors.primary}";
          label-focused-padding = 2;

          label-unfocused = "%icon%";
          label-unfocused-padding = 2;

          #label-unfocused-foreground = "${config.custom.shared.colors.primary}";
          #label-unfocused-background = "${config.custom.shared.colors.background-alt}";
          #label-unfocused-underline = "${config.custom.shared.colors.primary}";
          label-visible = "%icon%";
          label-visible-padding = 2;

          label-urgent = "%icon%";
          label-urgent-foreground = "${config.custom.shared.colors.alert}";
          label-urgent-padding = 2;

          #label-separator = "|";
          #label-separator-padding = 0;

          # Neither <label> nor <icon> can contain a semicolon (;)

        };

        settings = {
          screenchange-reload = true;
          pseudo-transparency = true;
        };
      };
      script = "

      for m in $(polybar --list-monitors | /run/current-system/sw/bin/cut -d \":\" -f1); do

        MONITOR=$m polybar --reload bottom &
      done
      ";
    };
  };
}
