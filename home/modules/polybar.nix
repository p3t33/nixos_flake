{ config, pkgs,  ... }:

let
  ws1 = "1:  Firefox";
  ws2 = "2:  VSCode";
  ws3 = "3:  Cherrytree";
  ws4 = "4:  Chrome";
  ws6 = "5:  terminal";
  ws5 = "6:  BuildServer";
  ws7 = "7";
  ws8 = "8:  VM";
  ws9 = "9: VPN";
  ws10 = "10";

in
{
    services.polybar = {
    enable = true;
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
        foreground = "${config.userDefinedGlobalVariables.colors.foreground}";
        background = "${config.userDefinedGlobalVariables.colors.background}";

        # underline / overline
        line-size = 2;
        line-color = "${config.userDefinedGlobalVariables.colors.primary}";
        border-size = 0;
        padding = 0;
        module-margin = 1;


        font-0 = "JetBrainsMono Nerd Font:style=Regular:size=15;4";
        # Just sticking them together in the center for now
        modules-left = "i3";
        modules-right = "filesystem memory cpu wlan battery date allen_tx ";
        #modules-center = "i3";

        cursor-click = "pointer";
        cursor-scroll = "ns-resize";
        enable-ipc = true;
      };

      "module/allen_tx" = {
        type = "custom/script";
        exec = "TZ=America/Chicago /run/current-system/sw/bin/date +\"US: %H:%M\"";
        interval = 30;
        format-foreground = "${config.userDefinedGlobalVariables.colors.foreground}";
        format-background = "${config.userDefinedGlobalVariables.colors.background-alt}";
        format-padding = 2;
      };

      "module/oslogo" = {
          type = "custom/text";
          content = " NixOS";
          content-foreground = "${config.userDefinedGlobalVariables.colors.foreground}";
          content-background = "${config.userDefinedGlobalVariables.colors.background-alt}";
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

          icon-0 = "${ws10};";
          icon-1 = "${ws1};";
          icon-2 = "${ws2};";
          icon-3 = "${ws3};";
          icon-4 = "${ws4};";
          icon-5 = "${ws5};";
          icon-6 = "${ws6};";
          icon-7 = "${ws7};";
          icon-8 = "${ws8};";
          icon-9 = "${ws9};";
          icon-default = "";

          format = "<label-state>";

          label-active = "%name%";
          label-active-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
          label-active-background = "${config.userDefinedGlobalVariables.colors.background-alt}";
          label-active-underline = "${config.userDefinedGlobalVariables.colors.primary}";

          label-occupied = "%name%";

          label-urgent = "%icon%";
          label-urgent-foreground = "${config.userDefinedGlobalVariables.colors.alert}";

          label-empty = "%icon%";
          label-empty-foreground = "${config.userDefinedGlobalVariables.colors.disabled}";

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
          format-prefix-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
          label-empty = "NixOS";
        };

        "module/filesystem" = {
          type = "internal/fs";
          interval = 25;
          mount-0 = "/";
          label-mounted = "%{F${config.userDefinedGlobalVariables.colors.primary}}DISK:%{F-} %percentage_used:2%%";
          label-unmounted = "%mountpoint% not mounted";
          label-unmounted-foreground = "${config.userDefinedGlobalVariables.colors.disabled}";
        };

        "module/memory" = {
          type = "internal/memory";
          interval = 2;
          format-prefix = "RAM: ";
          format-prefix-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
          label = "%percentage_used:2%%";
        };

        "module/cpu" = {
          type = "internal/cpu";
          interval = 2;
          format-prefix = "CPU: ";
          format-prefix-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
          label = "%percentage:2%%";
        };

        "module/wlan" = {
          type = "internal/network";
          interval = 5;
          interface-type = "wireless";
          format-connected = "<label-connected>";
          format-disconnected = "<label-disconnected>";
          label-connected = "on";
          label-disconnected = "off";
          format-connected-prefix = "直 ";
          format-connected-prefix-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
          format-disconnected-prefix = "睊 ";
          format-disconnected-foreground = "${config.userDefinedGlobalVariables.colors.disabled}";
          format-disconnected-prefix-foreground = "${config.userDefinedGlobalVariables.colors.disabled}";
        };

        "module/date" = {
          type = "internal/date";
          interval = 1;
          date = "%d-%m-%Y  %H:%M" ;
          label = "%date%";
          format = "<label>";
          format-prefix = " ";
          format-foreground = "${config.userDefinedGlobalVariables.colors.foreground}";
          format-background = "${config.userDefinedGlobalVariables.colors.background-alt}";
          format-padding = 2;
        };

        "module/battery" = {
          type = "internal/battery";
          # ls -1 /sys/class/power_supply/
          battery = "BAT0";
          adapter = "AC";

          # This is useful in case the battery never
          # reports 100% charge
          full-at = 95;

          label-charging = "%percentage:3%%";
          label-discharging = "%percentage:3%%";
          label-full = "%percentage:3%%";
          format-charging = "<label-charging>";
          format-discharging = "<label-discharging>";
          format-full = "<label-full>";
          format-charging-prefix = " ";
          format-discharging-prefix = " ";
          format-full-prefix = " ";
          format-charging-prefix-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
          format-discharging-prefix-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
          format-full-prefix-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
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
        ws-icon-0 = "${ws10};";
        ws-icon-1 = "${ws1};";
        ws-icon-2 = "${ws2};";
        ws-icon-3 = "${ws3};";
        ws-icon-4 = "${ws4};";
        ws-icon-5 = "${ws5};";
        ws-icon-6 = "${ws6};";
        ws-icon-7 = "${ws7};";
        ws-icon-8 = "${ws8};";
        ws-icon-9 = "${ws9};";
        ws-icon-default = "";

        format = "<label-state> <label-mode>";
        label-dimmed-underline= "${config.userDefinedGlobalVariables.colors.background}";

        label-mode = "%mode%";
        label-mode-padding = 2;

        label-focused = "%name%";
        label-focused-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
        label-focused-background = "${config.userDefinedGlobalVariables.colors.background-alt}";
        label-focused-underline = "${config.userDefinedGlobalVariables.colors.primary}";
        label-focused-padding = 2;

        label-unfocused = "%icon%";
        label-unfocused-padding = 2;

        #label-unfocused-foreground = "${config.userDefinedGlobalVariables.colors.primary}";
        #label-unfocused-background = "${config.userDefinedGlobalVariables.colors.background-alt}";
        #label-unfocused-underline = "${config.userDefinedGlobalVariables.colors.primary}";
        label-visible = "%icon%";
        label-visible-padding = 2;


        label-urgent = "%icon%";
        label-urgent-foreground = "${config.userDefinedGlobalVariables.colors.alert}";
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
}
