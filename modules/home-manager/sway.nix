{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  audioEnabled = osConfig.services.pipewire.enable && osConfig.services.pipewire.wireplumber.enable;
  cfg = config.wayland.windowManager.sway;
  cliphistMenu = import ./rofi/scripts/cliphist.nix { inherit config lib pkgs; };
  flameshot = lib.getExe config.services.flameshot.package;
  flameshotWindowCommands = lib.optionals config.services.flameshot.enable [
    {
      criteria.app_id = "^flameshot$";
      command = "border pixel 0, floating enable, fullscreen disable, move absolute position 0 0";
    }
  ];
  mod = "Mod4";
  moolticuteWindowCommands = lib.optionals config.custom.programs.moolticute.enable [
    {
      criteria.app_id = "^com[.]themooltipass[.]$";
      command = "floating disable";
    }
  ];
  rofi = lib.getExe config.programs.rofi.finalPackage;
  rofiPowerMenu = lib.getExe pkgs.rofi-power-menu;
  wpctl = lib.getExe' osConfig.services.pipewire.wireplumber.package "wpctl";
  ws1 = config.custom.sway.workspaces.ws1;
  ws2 = config.custom.sway.workspaces.ws2;
  ws3 = config.custom.sway.workspaces.ws3;
  ws4 = config.custom.sway.workspaces.ws4;
  ws5 = config.custom.sway.workspaces.ws5;
  ws6 = config.custom.sway.workspaces.ws6;
  ws7 = config.custom.sway.workspaces.ws7;
  ws8 = config.custom.sway.workspaces.ws8;
  ws9 = config.custom.sway.workspaces.ws9;
  ws10 = config.custom.sway.workspaces.ws10;
in
{
  options.custom.sway = {
    autostart = {
      firefox = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to start Firefox with Sway";
      };
      googleChrome = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to start Google Chrome with Sway";
      };
    };

    workspacesIcons =
      lib.mapAttrs
        (
          name: default:
          lib.mkOption {
            inherit default;
            type = lib.types.str;
            description = "Icon for the ${name} Sway workspace label";
          }
        )
        {
          firefox = "";
          code = "";
          cherrytree = "";
          chrome = "";
          terminal = "";
          buildserver = "";
          default = "";
        };

    workspaces =
      lib.mapAttrs
        (
          name: default:
          lib.mkOption {
            inherit default;
            type = lib.types.str;
            description = "Definition for Sway workspace ${name}";
          }
        )
        {
          ws1 = "1: ${config.custom.sway.workspacesIcons.firefox} Firefox";
          ws2 = "2: ${config.custom.sway.workspacesIcons.code} Code";
          ws3 = "3: ${config.custom.sway.workspacesIcons.cherrytree} Cherrytree";
          ws4 = "4: ${config.custom.sway.workspacesIcons.chrome} Chrome";
          ws5 = "5: ${config.custom.sway.workspacesIcons.buildserver} BuildServer";
          ws6 = "6: ${config.custom.sway.workspacesIcons.terminal} Terminal";
          ws7 = "7";
          ws8 = "8: ${config.custom.sway.workspacesIcons.default} VM";
          ws9 = "9: VPN";
          ws10 = "10";
        };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.custom.sway.autostart.firefox || config.programs.firefox.enable;
        message = "custom.sway.autostart.firefox requires programs.firefox";
      }
    ];

    home.packages = with pkgs; [
      nwg-displays
      wl-clipboard
    ];

    wayland.windowManager.sway = {
      package = osConfig.programs.sway.package;

      config = {
        modifier = mod;
        terminal = lib.getExe config.programs.alacritty.package;
        bars = [ ];
        bindkeysToCode = true;

        fonts = {
          names = [ config.custom.shared.font.sansSerif ];
          size = 20.0;
        };

        input."type:keyboard" = {
          xkb_layout = "us,il";
          xkb_options = "grp:caps_toggle";
        };

        keybindings = lib.mkOptionDefault {
          "${mod}+d" = if config.programs.rofi.enable then "exec ${rofi} -modes drun -show drun" else null;
          "${mod}+x" = if config.programs.rofi.enable then "exec ${rofi} -modes ssh -show ssh" else null;
          "${mod}+z" = if config.programs.rofi.enable then "exec ${rofi} -modes emoji -show emoji" else null;
          "${mod}+y" = if config.programs.rofi.enable then "exec ${rofi} -modes calc -show calc" else null;
          "${mod}+p" =
            if config.programs.rofi.enable then
              "exec ${rofi} -modes power-menu:${rofiPowerMenu} -show power-menu"
            else
              null;
          "${mod}+b" =
            if config.programs.rofi.enable then
              "exec ${rofi} -modes buku-bookmarks:rofi-buku-bookmakrs -show buku-bookmarks"
            else
              null;
          "${mod}+c" =
            if config.services.cliphist.enable && config.programs.rofi.enable then
              "exec ${cliphistMenu}"
            else
              null;
          "XF86AudioRaiseVolume" =
            if audioEnabled then "exec ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%+ --limit 1.0" else null;
          "XF86AudioLowerVolume" =
            if audioEnabled then "exec ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-" else null;
          "--no-repeat XF86AudioMute" =
            if audioEnabled then "exec ${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle" else null;
          "--no-repeat XF86AudioMicMute" =
            if audioEnabled then "exec ${wpctl} set-mute @DEFAULT_AUDIO_SOURCE@ toggle" else null;
          "Print" = if config.services.flameshot.enable then "exec ${flameshot} gui" else null;
          "${mod}+Shift+Return" =
            "exec ${lib.getExe config.programs.alacritty.package} -e zsh -i -c 'tmux a || tmux new'";

          "${mod}+Mod1+1" = "[con_mark=\"one\"] focus";
          "${mod}+Mod1+2" = "[con_mark=\"two\"] focus";
          "${mod}+Mod1+3" = "[con_mark=\"three\"] focus";
          "${mod}+Mod1+4" = "[con_mark=\"four\"] focus";
          "${mod}+Mod1+Shift+1" = "mark one";
          "${mod}+Mod1+Shift+2" = "mark two";
          "${mod}+Mod1+Shift+3" = "mark three";
          "${mod}+Mod1+Shift+4" = "mark four";

          "${mod}+s" = "split v";
          "${mod}+v" = "split h";

          "${mod}+1" = "workspace number ${ws1}";
          "${mod}+2" = "workspace number ${ws2}";
          "${mod}+3" = "workspace number ${ws3}";
          "${mod}+4" = "workspace number ${ws4}";
          "${mod}+5" = "workspace number ${ws5}";
          "${mod}+6" = "workspace number ${ws6}";
          "${mod}+7" = "workspace number ${ws7}";
          "${mod}+8" = "workspace number ${ws8}";
          "${mod}+9" = "workspace number ${ws9}";
          "${mod}+0" = "workspace number ${ws10}";

          "${mod}+Shift+1" = "move container to workspace number ${ws1}";
          "${mod}+Shift+2" = "move container to workspace number ${ws2}";
          "${mod}+Shift+3" = "move container to workspace number ${ws3}";
          "${mod}+Shift+4" = "move container to workspace number ${ws4}";
          "${mod}+Shift+5" = "move container to workspace number ${ws5}";
          "${mod}+Shift+6" = "move container to workspace number ${ws6}";
          "${mod}+Shift+7" = "move container to workspace number ${ws7}";
          "${mod}+Shift+8" = "move container to workspace number ${ws8}";
          "${mod}+Shift+9" = "move container to workspace number ${ws9}";
          "${mod}+Shift+0" = "move container to workspace number ${ws10}";

          "${mod}+Control+Shift+1" = "move container to workspace number ${ws1}; workspace number ${ws1}";
          "${mod}+Control+Shift+2" = "move container to workspace number ${ws2}; workspace number ${ws2}";
          "${mod}+Control+Shift+3" = "move container to workspace number ${ws3}; workspace number ${ws3}";
          "${mod}+Control+Shift+4" = "move container to workspace number ${ws4}; workspace number ${ws4}";
          "${mod}+Control+Shift+5" = "move container to workspace number ${ws5}; workspace number ${ws5}";
          "${mod}+Control+Shift+6" = "move container to workspace number ${ws6}; workspace number ${ws6}";
          "${mod}+Control+Shift+7" = "move container to workspace number ${ws7}; workspace number ${ws7}";
          "${mod}+Control+Shift+8" = "move container to workspace number ${ws8}; workspace number ${ws8}";
          "${mod}+Control+Shift+9" = "move container to workspace number ${ws9}; workspace number ${ws9}";
          "${mod}+Control+Shift+0" = "move container to workspace number ${ws10}; workspace number ${ws10}";

          "${mod}+bracketleft" = "focus output left";
          "${mod}+bracketright" = "focus output right";
          "${mod}+Shift+bracketleft" = "move workspace to output left";
          "${mod}+Shift+bracketright" = "move workspace to output right";

          "${mod}+Tab" = "workspace back_and_forth";
          "${mod}+Shift+c" = "kill";
          "${mod}+Shift+r" = "reload";
          "${mod}+Shift+e" = null;
          "${mod}+Shift+q" =
            "exec ${lib.getExe' cfg.package "swaynag"} -t warning -m 'Exit Sway? This will close your graphical session.' -b 'Exit' 'swaymsg exit'";
        };

        assigns = {
          "${ws1}" = [ { app_id = "^firefox$"; } ];
          "${ws3}" = [ { app_id = "^cherrytree$"; } ];
          "${ws4}" = [ { app_id = "^google-chrome$"; } ];
        };

        window.commands = flameshotWindowCommands ++ moolticuteWindowCommands;

        startup =
          lib.optionals config.custom.sway.autostart.googleChrome [
            {
              command = lib.getExe' pkgs.google-chrome "google-chrome-stable";
              always = false;
            }
          ]
          ++ lib.optionals config.custom.sway.autostart.firefox [
            {
              command = lib.getExe config.programs.firefox.package;
              always = false;
            }
          ];

        colors = {
          focused = {
            text = config.custom.shared.colors.text;
            background = config.custom.shared.colors.background-alt;
            border = config.custom.shared.colors.primary;
            childBorder = config.custom.shared.colors.primary;
            indicator = config.custom.shared.colors.alert;
          };

          focusedInactive = {
            text = config.custom.shared.colors.inactive-text;
            background = config.custom.shared.colors.inactive-bg;
            border = config.custom.shared.colors.inactive-bg;
            childBorder = config.custom.shared.colors.secondary;
            indicator = config.custom.shared.colors.alert;
          };

          unfocused = {
            text = config.custom.shared.colors.inactive-text;
            background = config.custom.shared.colors.inactive-bg;
            border = config.custom.shared.colors.background;
            childBorder = config.custom.shared.colors.background;
            indicator = config.custom.shared.colors.alert;
          };
        };
      };
    };
  };
}
