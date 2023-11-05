{ config, lib, pkgs, ... }:

let
  mod = "Mod4";
  ws1 = "${config.userDefinedGlobalVariables.workspaces.ws1}";
  ws2 = "${config.userDefinedGlobalVariables.workspaces.ws2}";
  ws3 = "${config.userDefinedGlobalVariables.workspaces.ws3}";
  ws4 = "${config.userDefinedGlobalVariables.workspaces.ws4}";
  ws5 = "${config.userDefinedGlobalVariables.workspaces.ws5}";
  ws6 = "${config.userDefinedGlobalVariables.workspaces.ws6}";
  ws7 = "${config.userDefinedGlobalVariables.workspaces.ws7}";
  ws8 = "${config.userDefinedGlobalVariables.workspaces.ws8}";
  ws9 = "${config.userDefinedGlobalVariables.workspaces.ws9}";
  ws10 = "${config.userDefinedGlobalVariables.workspaces.ws10}";


in
{
  # uncomment if you would like to use the i3status bar
  #imports = [./i3/bars.nix];

  xsession.windowManager.i3 = {
    enable = true;

    config = {
      modifier = mod;
      defaultWorkspace = "workspace number 1";

      fonts = {
        names = [ config.userDefinedGlobalVariables.font.sansSerif ];
        # There is also an optin to set style for the font
        # style = "Bold Italic";
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
          command = "${pkgs.nitrogen}/bin/nitrogen --set-scaled  ${config.xdg.configHome}/${config.userDefinedGlobalVariables.wallpaperOut} --head=-1";
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
        "${mod}+Return" = "exec --no-startup-id ${pkgs.alacritty}/bin/alacritty";

        # lunch alacritty with tmux attach.
        "${mod}+Shift+Return" = "exec --no-startup-id ${pkgs.alacritty}/bin/alacritty -e  zsh -i -c 'tmux a || tmux new'";

        # kill window.
        "${mod}+Shift+c" = "kill";

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

        # switch to workspace
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

        # move focused container to workspace and go there
        "${mod}+Control+Shift+1" = "move container to workspace number ${ws1}; workspace ${ws1}";
        "${mod}+Control+Shift+2" = "move container to workspace number ${ws2}; workspace ${ws2}";
        "${mod}+Control+Shift+3" = "move container to workspace number ${ws3}; workspace ${ws3}";
        "${mod}+Control+Shift+4" = "move container to workspace number ${ws4}; workspace ${ws4}";
        "${mod}+Control+Shift+5" = "move container to workspace number ${ws5}; workspace ${ws5}";
        "${mod}+Control+Shift+6" = "move container to workspace number ${ws6}; workspace ${ws6}";
        "${mod}+Control+Shift+7" = "move container to workspace number ${ws7}; workspace ${ws7}";
        "${mod}+Control+Shift+8" = "move container to workspace number ${ws8}; workspace ${ws8}";
        "${mod}+Control+Shift+9" = "move container to workspace number ${ws9}; workspace ${ws9}";
        "${mod}+Control+Shift+10" = "move container to workspace number ${ws10}; workspace ${ws10}";

        # Move focus to different outputs(monitors)
        "${mod}+bracketleft" = "focus output left";
        "${mod}+bracketright" = "focus output right";

        # Move workspace to different outputs(monitors)
        "${mod}+Shift+bracketleft" = "move workspace to output left";
        "${mod}+Shift+bracketright" = "move workspace to output right";


        # Alternating between two most recent workspaces
        "${mod}+Tab" = "workspace back_and_forth";

        # reload the configuration file
        "${mod}+Shift+r" = "reload";

        ## restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
        "${mod}+q" = "restart";

        # exit i3 (logs you out of your X session)
        "${mod}+Shift+q" = "exec \"i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' 'i3-msg exit'\"";

        #"XF86AudioRaiseVolume" =  "exec --no-startup-id amixer sset Master 5%+";
        #"XF86AudioLowerVolume" = "exec --no-startup-id amixer sset Master 5%-";
        #"XF86AudioMute" = "exec --no-startup-id amixer sset Master toggle";
        #"XF86AudioMicMute" = "exec --no-startup-id amixer set Capture toggle";
      };

      colors = {
        focused = {
          text = config.userDefinedGlobalVariables.colors.text;
          background = config.userDefinedGlobalVariables.colors.background-alt;
          border = config.userDefinedGlobalVariables.colors.primary;
          childBorder = config.userDefinedGlobalVariables.colors.primary;
          indicator = config.userDefinedGlobalVariables.colors.alert;
        };

        focusedInactive = {
          text = config.userDefinedGlobalVariables.colors.inactive-text;
          background = config.userDefinedGlobalVariables.colors.inactive-bg;
          border = config.userDefinedGlobalVariables.colors.inactive-bg;
          childBorder = config.userDefinedGlobalVariables.colors.secondary;
          indicator = config.userDefinedGlobalVariables.colors.alert;
        };

        unfocused = {
          text = config.userDefinedGlobalVariables.colors.inactive-text;
          background = config.userDefinedGlobalVariables.colors.inactive-bg;
          border = config.userDefinedGlobalVariables.colors.background;
          childBorder = config.userDefinedGlobalVariables.colors.background;
          indicator = config.userDefinedGlobalVariables.colors.alert;
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
