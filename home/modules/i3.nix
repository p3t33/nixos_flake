{ config, lib, pkgs, ... }:

let 
  colors = rec {
    background = "#312f2f";
    background-alt = "#3b4354";

    foreground = "#F1FAEE";

    primary = "#08D9D6";
    secondary = "#047672";
    alert = "#ff2e63";
    disabled = "#707880";

    bg = "#2f343f";
    inactive-bg = "#2f343f";
    text = "#f3f4f5";
    inactive-text = "#676E70";
    urgent-bg = "#E53935";
    
  };

  mod = "Mod4";
  ws1 = "1:  Firefox";
  ws2 = "2:  VSCode";
  ws3 = "3:  Cherrytree";
  ws4 = "4:  Chrome";
  ws5 = "5:  BuildServer";
  ws6 = "6:  terminal";
  ws7 = "7";
  ws8 = "8:  VM";
  ws9 = "9: VPN";
  ws10 = "10";

  wallpaperOut = "wallpaper/mountain.jpg";

in {
  # uncomment if you would like to use the i3status bar
  #imports = [./i3/bars.nix];

  xsession.windowManager.i3 = {
    enable = true;
    
    config = {
      modifier = mod;
      defaultWorkspace = "workspace number 1";

      fonts = {
        names = [ "nerdfonts" ];
        size = 20.0;
      };

      # using polybar so this isn't required.
      # if the imports varible is commented in then the bars = [] needs
      # to be commented out or there will be conflicts. and nixos will
      # fail to build.
      bars = [];

      # mod + d
      menu = "${pkgs.rofi}/bin/rofi -modi drun -show drun";

      startup = [
        {
          command = "systemctl --user restart polybar";
          always = true;
          notification = false;
        }
        {
          command = "${pkgs.vscode}/bin/code";

          # always = true --> exec_always
          # always = false --> exec
          always = false;
          # notification = false --> --no-startup-id
          notification = false;
        }
        {
          command = "${pkgs.cherrytree}/bin/cherrytree";
          always = false;
          notification = false;
        }
        {
          command = "${pkgs.firefox}/bin/firefox";
          always = false;
          notification = false;
        }
        {
          command = "${pkgs.google-chrome}/bin/google-chrome-stable";
          always = false;
          notification = false;
        }
        {
          command = "${pkgs.slack}/bin/slack";
          always = false;
          notification = false;
        }
        {
          command = "${pkgs.nitrogen}/bin/nitrogen --set-scaled  ${config.xdg.configHome}/${wallpaperOut} --head=-1";
          always = true;
          notification = false;
        }
        {
          command = "pgrep sxhkd || ${pkgs.sxhkd}/bin/sxhkd";
          always = true;
          notification = false;
        }
      ];

      assigns = {
        "${ws1}" = [{ class = "firefox"; }];
        "${ws2}" = [{ class = "Code"; }];
        "${ws3}" = [{ class = "Cherrytree"; }];
        "${ws4}" = [{ class = "Google-chrome"; } { class = "Slack"; }];
      };


      keybindings = lib.mkOptionDefault {
        # lunch alacritty.
        "${mod}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";

        # kill window.
        "${mod}+Shift+c" = "kill";



        #"${mod}+x" = ''exec --no-startup-id "rofi -modi ssh -show ssh"'';
        #"${mod}+z" = ''exec --no-startup-id "rofi -modi emoji -show emoji"'';
        #"${mod}+c" = ''exec --no-startup-id "rofi -modi calc -show calc"'';
        #"${mod}+Shift+x" = "exec sh -c '${pkgs.i3lock}/bin/i3lock -c 222222 & sleep 5 && xset dpms force of'";

        # Change focus
        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";

        # Move focused window
        "${mod}+Shift+h" = "move left";
        "${mod}+Shift+j" = "move down";
        "${mod}+Shift+k" = "move up";
        "${mod}+Shift+l" = "move right";

        # Split in horizontal orientation.
        "${mod}+s" = "split v";

        # Split in vertical orientation.
        "${mod}+v" = "split h";

        # enter full screen mode for focused window
        "${mod}+f" = "fullscreen toggle";

        # change container layout (stacked, tabbed, toggle split)
        #${mod}+s" = "layout stacking";
        "${mod}+w" = "layout tabbed";
        "${mod}+e" = "layout toggle split";

        # switch to worspace
        "${mod}+1" = "workspace number ${ws1}";
        "${mod}+2" = "workspace number ${ws2}";
        "${mod}+3" = "workspace number ${ws3}";
        "${mod}+4" = "workspace number ${ws4}";
        "${mod}+5" = "workspace number ${ws5}";
        "${mod}+6" = "workspace number ${ws6}";
        "${mod}+7" = "workspace number ${ws7}";
        "${mod}+8" = "workspace number ${ws8}";
        "${mod}+9" = "workspace number ${ws9}";
        "${mod}+10" = "workspace number ${ws10}";

        # move focused container to workspace
        "${mod}+Shift+1" = "move container to workspace number ${ws1}";
        "${mod}+Shift+2" = "move container to workspace number ${ws2}";
        "${mod}+Shift+3" = "move container to workspace number ${ws3}";
        "${mod}+Shift+4" = "move container to workspace number ${ws4}";
        "${mod}+Shift+5" = "move container to workspace number ${ws5}";
        "${mod}+Shift+6" = "move container to workspace number ${ws6}";
        "${mod}+Shift+7" = "move container to workspace number ${ws7}";
        "${mod}+Shift+8" = "move container to workspace number ${ws8}";
        "${mod}+Shift+9" = "move container to workspace number ${ws9}";
        "${mod}+Shift+10" = "move container to workspace number ${ws10}";

        # Move workspace to different outputs(monitors)
        "${mod}+Shift+comma" = "move workspace to output left";
        "${mod}+Shift+period" = "move workspace to output right";

        # reload the configuration file
        "${mod}+Shift+r" = "reload";

        ## restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
        "${mod}+q" = "restart";

        # exit i3 (logs you out of your X session)
        "${mod}+Shift+q" = "exec \"i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' 'i3-msg exit'\"";

        "XF86AudioRaiseVolume" =  "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10%";
        "XF86AudioLowerVolume" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10%";
        "XF86AudioMute" = "exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle";
        "XF86AudioMicMute" = "exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle";
      };

      colors = {
        focused = {
          text = colors.text;
          background = colors.background-alt;
          border = colors.primary;
          childBorder = colors.primary;
          indicator = colors.alert;
        };

        focusedInactive = {
          text = colors.inactive-text;
          background = colors.inactive-bg;
          border = colors.inactive-bg;
          childBorder = colors.secondary;
          indicator = colors.alert;
        };

        unfocused = {
          text = colors.inactive-text;
          background = colors.inactive-bg;
          border = colors.background;
          childBorder = colors.background;
          indicator = colors.alert;
          };
        };

      # i3wm like other tiling window managers defines where and how bars are
      # set(position, font...). i3wm has a builtin status bar called i3bar.
      #
      # i3wm isn't responsible for what informtion is displayed inside the bar
      # that is defined at the status bar level(E.g i3status). i3bar might be
      # an exception to the rule.
      #
      # nixOS specifics:
      #
      # If a standalone bar such as i3status has home-manger configuration and
      # it is enabled but no bars option is set for it, it will be present
      # and default settings will be applied for it. 
      
      modes = {
        resize = {

            "h" = "resize shrink width 10 px or 10 ppt";
            "j" = "resize grow height 10 px or 10 ppt";
            "k" = "resize shrink height 10 px or 10 ppt";
            "l" = "resize grow width 10 px or 10 ppt";
            
            "Left" = "resize shrink width 10 px or 10 ppt";
            "Down" = "resize grow height 10 px or 10 ppt";
            "Up" = "resize shrink height 10 px or 10 ppt";
            "Right" = "resize grow width 10 px or 10 ppt";

            "Escape" = "mode default";
            "Return" = "mode default";
        };
      };

    };
  };
}
