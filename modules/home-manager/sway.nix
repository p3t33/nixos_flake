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
  colors = config.custom.shared.omarchyColors;
  outlines = config.custom.shared.outlineColors;
  cliphistMenu = import ./rofi/scripts/cliphist.nix { inherit config lib pkgs; };
  flameshotFocused = lib.getExe config.custom.programs.flameshot.focusedScreenshotPackage;
  flameshotWindowCommands = lib.optionals config.services.flameshot.enable [
    {
      criteria.app_id = "^flameshot$";
      command = "border pixel 0, floating enable, fullscreen disable, move absolute position 0 0";
    }
  ];
  inhibited = "--inhibited";
  mod = "Mod4";
  moolticuteWindowCommands = lib.optionals config.custom.programs.moolticute.enable [
    {
      criteria.app_id = "^com[.]themooltipass[.]$";
      command = "floating disable";
    }
  ];
  rofi = lib.getExe config.programs.rofi.finalPackage;
  emojiPicker = pkgs.writeShellScript "rofimoji-picker" ''
    export PATH=${lib.makeBinPath [ config.programs.rofi.finalPackage pkgs.wl-clipboard pkgs.wtype ]}:"$PATH"
    exec ${lib.getExe pkgs.rofimoji} --selector rofi --clipboarder wl-copy --typer wtype \
      --action type copy --max-recent 10 --use-icons --skin-tone neutral \
      --selector-args="-no-sort -theme-str 'window { width: 600px; }'"
  '';
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
        # default monitor configurations shared by all gui machines that
        # use sway.
        output = {
          "LG Electronics LG HDR WQHD 0x0000BF89" = {
            mode = lib.mkDefault "3440x1440@59.973Hz";
            position = lib.mkDefault "0 0";
            scale = lib.mkDefault "1";
            transform = lib.mkDefault "normal";
          };
          "Dell Inc. DELL P2314H J8J3141HK8DL" = {
            mode = lib.mkDefault "1920x1080@60Hz";
            position = lib.mkDefault "3440 0";
            scale = lib.mkDefault "1";
            transform = lib.mkDefault "normal";
          };
        };

        modifier = mod;
        terminal = lib.getExe config.programs.wezterm.package;
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

        modes.resize = {
          "${inhibited} ${cfg.config.left}" = "resize shrink width 10 px";
          "${inhibited} ${cfg.config.down}" = "resize grow height 10 px";
          "${inhibited} ${cfg.config.up}" = "resize shrink height 10 px";
          "${inhibited} ${cfg.config.right}" = "resize grow width 10 px";
          "${inhibited} Left" = "resize shrink width 10 px";
          "${inhibited} Down" = "resize grow height 10 px";
          "${inhibited} Up" = "resize shrink height 10 px";
          "${inhibited} Right" = "resize grow width 10 px";
          "${inhibited} Escape" = "mode default";
          "${inhibited} Return" = "mode default";
        };

        keybindings = lib.mkOptionDefault {
          "${mod}+Return" = null;
          "${inhibited} ${mod}+Return" = "exec ${lib.getExe config.programs.wezterm.package}";
          "${mod}+Left" = null;
          "${inhibited} ${mod}+Left" = "focus left";
          "${mod}+Down" = null;
          "${inhibited} ${mod}+Down" = "focus down";
          "${mod}+Up" = null;
          "${inhibited} ${mod}+Up" = "focus up";
          "${mod}+Right" = null;
          "${inhibited} ${mod}+Right" = "focus right";
          "${mod}+a" = null;
          "${inhibited} ${mod}+a" = "focus parent";
          "${mod}+space" = null;
          "${inhibited} ${mod}+space" = "focus mode_toggle";
          "${mod}+Shift+${cfg.config.left}" = null;
          "${inhibited} ${mod}+Shift+${cfg.config.left}" = "move left";
          "${mod}+Shift+${cfg.config.down}" = null;
          "${inhibited} ${mod}+Shift+${cfg.config.down}" = "move down";
          "${mod}+Shift+${cfg.config.up}" = null;
          "${inhibited} ${mod}+Shift+${cfg.config.up}" = "move up";
          "${mod}+Shift+${cfg.config.right}" = null;
          "${inhibited} ${mod}+Shift+${cfg.config.right}" = "move right";
          "${mod}+Shift+Left" = null;
          "${inhibited} ${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = null;
          "${inhibited} ${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = null;
          "${inhibited} ${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = null;
          "${inhibited} ${mod}+Shift+Right" = "move right";
          "${mod}+Shift+space" = null;
          "${inhibited} ${mod}+Shift+space" = "floating toggle";
          "${mod}+e" = null;
          "${inhibited} ${mod}+e" = "layout toggle split";
          "${mod}+w" = null;
          "${inhibited} ${mod}+w" = "layout tabbed";
          "${mod}+r" = null;
          "${inhibited} ${mod}+r" = "mode resize";
          "${mod}+minus" = null;
          "${inhibited} ${mod}+minus" = "scratchpad show";
          "${mod}+Shift+minus" = null;
          "${inhibited} ${mod}+Shift+minus" = "move scratchpad";

          "${mod}+f" = null;
          "${inhibited} ${mod}+f" = "fullscreen toggle";
          "${mod}+d" = null;
          "${inhibited} ${mod}+d" =
            if config.programs.rofi.enable then "exec ${rofi} -modes drun -show drun" else null;
          "${inhibited} ${mod}+x" = if config.programs.rofi.enable then "exec ${rofi} -modes ssh -show ssh" else null;
          "${inhibited} ${mod}+g" = if config.programs.rofi.enable then "exec ${emojiPicker}" else null;
          "${inhibited} ${mod}+y" = if config.programs.rofi.enable then "exec ${rofi} -modes calc -show calc" else null;
          "${inhibited} ${mod}+p" =
            if config.programs.rofi.enable then
              "exec ${rofi} -modes 'power-menu:${rofiPowerMenu} --confirm=' -show power-menu"
            else
              null;
          "${mod}+b" = null;
          "${inhibited} ${mod}+b" =
            if config.programs.rofi.enable then
              "exec ${rofi} -modes buku-bookmarks:rofi-buku-bookmakrs -show buku-bookmarks"
            else
              null;
          "${inhibited} ${mod}+c" =
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
          "Print" = if config.services.flameshot.enable then "exec ${flameshotFocused}" else null;
          "${inhibited} ${mod}+Shift+Return" =
            "exec ${lib.getExe config.programs.wezterm.package} start -- ${lib.getExe pkgs.zsh} -i -c '${lib.getExe config.custom.scripts.tmuxSessionizer.package} --attach-last'";

          "${inhibited} ${mod}+Mod1+1" = "[con_mark=\"one\"] focus";
          "${inhibited} ${mod}+Mod1+2" = "[con_mark=\"two\"] focus";
          "${inhibited} ${mod}+Mod1+3" = "[con_mark=\"three\"] focus";
          "${inhibited} ${mod}+Mod1+4" = "[con_mark=\"four\"] focus";
          "${inhibited} ${mod}+Mod1+Shift+1" = "mark one";
          "${inhibited} ${mod}+Mod1+Shift+2" = "mark two";
          "${inhibited} ${mod}+Mod1+Shift+3" = "mark three";
          "${inhibited} ${mod}+Mod1+Shift+4" = "mark four";

          "${mod}+s" = null;
          "${inhibited} ${mod}+s" = "split v";
          "${mod}+v" = null;
          "${inhibited} ${mod}+v" = "split h";

          "${mod}+${cfg.config.left}" = null;
          "${mod}+${cfg.config.down}" = null;
          "${mod}+${cfg.config.up}" = null;
          "${mod}+${cfg.config.right}" = null;
          "${inhibited} ${mod}+${cfg.config.left}" = "focus left";
          "${inhibited} ${mod}+${cfg.config.down}" = "focus down";
          "${inhibited} ${mod}+${cfg.config.up}" = "focus up";
          "${inhibited} ${mod}+${cfg.config.right}" = "focus right";

          "${mod}+1" = null;
          "${mod}+2" = null;
          "${mod}+3" = null;
          "${mod}+4" = null;
          "${mod}+5" = null;
          "${mod}+6" = null;
          "${mod}+7" = null;
          "${mod}+8" = null;
          "${mod}+9" = null;
          "${mod}+0" = null;
          "${inhibited} ${mod}+1" = "workspace number ${ws1}";
          "${inhibited} ${mod}+2" = "workspace number ${ws2}";
          "${inhibited} ${mod}+3" = "workspace number ${ws3}";
          "${inhibited} ${mod}+4" = "workspace number ${ws4}";
          "${inhibited} ${mod}+5" = "workspace number ${ws5}";
          "${inhibited} ${mod}+6" = "workspace number ${ws6}";
          "${inhibited} ${mod}+7" = "workspace number ${ws7}";
          "${inhibited} ${mod}+8" = "workspace number ${ws8}";
          "${inhibited} ${mod}+9" = "workspace number ${ws9}";
          "${inhibited} ${mod}+0" = "workspace number ${ws10}";

          "${mod}+Shift+1" = null;
          "${inhibited} ${mod}+Shift+1" = "move container to workspace number ${ws1}";
          "${mod}+Shift+2" = null;
          "${inhibited} ${mod}+Shift+2" = "move container to workspace number ${ws2}";
          "${mod}+Shift+3" = null;
          "${inhibited} ${mod}+Shift+3" = "move container to workspace number ${ws3}";
          "${mod}+Shift+4" = null;
          "${inhibited} ${mod}+Shift+4" = "move container to workspace number ${ws4}";
          "${mod}+Shift+5" = null;
          "${inhibited} ${mod}+Shift+5" = "move container to workspace number ${ws5}";
          "${mod}+Shift+6" = null;
          "${inhibited} ${mod}+Shift+6" = "move container to workspace number ${ws6}";
          "${mod}+Shift+7" = null;
          "${inhibited} ${mod}+Shift+7" = "move container to workspace number ${ws7}";
          "${mod}+Shift+8" = null;
          "${inhibited} ${mod}+Shift+8" = "move container to workspace number ${ws8}";
          "${mod}+Shift+9" = null;
          "${inhibited} ${mod}+Shift+9" = "move container to workspace number ${ws9}";
          "${mod}+Shift+0" = null;
          "${inhibited} ${mod}+Shift+0" = "move container to workspace number ${ws10}";

          "${inhibited} ${mod}+Control+Shift+1" = "move container to workspace number ${ws1}; workspace number ${ws1}";
          "${inhibited} ${mod}+Control+Shift+2" = "move container to workspace number ${ws2}; workspace number ${ws2}";
          "${inhibited} ${mod}+Control+Shift+3" = "move container to workspace number ${ws3}; workspace number ${ws3}";
          "${inhibited} ${mod}+Control+Shift+4" = "move container to workspace number ${ws4}; workspace number ${ws4}";
          "${inhibited} ${mod}+Control+Shift+5" = "move container to workspace number ${ws5}; workspace number ${ws5}";
          "${inhibited} ${mod}+Control+Shift+6" = "move container to workspace number ${ws6}; workspace number ${ws6}";
          "${inhibited} ${mod}+Control+Shift+7" = "move container to workspace number ${ws7}; workspace number ${ws7}";
          "${inhibited} ${mod}+Control+Shift+8" = "move container to workspace number ${ws8}; workspace number ${ws8}";
          "${inhibited} ${mod}+Control+Shift+9" = "move container to workspace number ${ws9}; workspace number ${ws9}";
          "${inhibited} ${mod}+Control+Shift+0" = "move container to workspace number ${ws10}; workspace number ${ws10}";

          "${inhibited} ${mod}+bracketleft" = "focus output left";
          "${inhibited} ${mod}+bracketright" = "focus output right";
          "${inhibited} ${mod}+Shift+bracketleft" = "move workspace to output left";
          "${inhibited} ${mod}+Shift+bracketright" = "move workspace to output right";

          "${inhibited} ${mod}+Tab" = "workspace back_and_forth";
          "${mod}+Shift+c" = null;
          "${inhibited} ${mod}+Shift+c" = "kill";
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

        window.border = 3;
        window.titlebar = false;
        floating.border = 3;
        floating.titlebar = false;
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
            text = colors.foreground;
            background = colors.background;
            border = outlines.active;
            childBorder = outlines.active;
            indicator = outlines.active;
          };

          focusedInactive = {
            text = colors.foreground;
            background = colors.background;
            border = outlines.inactive;
            childBorder = outlines.inactive;
            indicator = outlines.active;
          };

          unfocused = {
            text = colors.foreground;
            background = colors.background;
            border = outlines.inactive;
            childBorder = outlines.inactive;
            indicator = outlines.inactive;
          };
        };
      };
    };
  };
}
