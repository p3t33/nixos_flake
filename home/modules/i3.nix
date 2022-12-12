{ config, lib, pkgs, ... }:

let 
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

in {
  xsession.windowManager.i3 = {
    enable = true;
    
    config = {
      modifier = mod;
      defaultWorkspace = "workspace number 1";

      fonts = {
        names = [ "nerdfonts" ];
        size = 20.0;
      };

      menu = "${pkgs.rofi}/bin/rofi -show drun -show-icons";

      startup = [
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
      ];

      assigns = {
        "${ws1}" = [{ class = "firefox"; }];
        "${ws2}" = [{ class = "Code"; }];
        "${ws3}" = [{ class = "Cherrytree"; }];
        "${ws4}" = [{ class = "Google-chrome"; }];
      };


      keybindings = lib.mkOptionDefault {
        # lunch alacritty.
        "${mod}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";

        # kill window.
        "${mod}+Shift+c" = "kill";



        "${mod}+p" = "exec ${pkgs.dmenu}/bin/dmenu_run";
        "${mod}+x" = "exec sh -c '${pkgs.maim}/bin/maim -s | xclip -selection clipboard -t image/png'";
        "${mod}+Shift+x" = "exec sh -c '${pkgs.i3lock}/bin/i3lock -c 222222 & sleep 5 && xset dpms force of'";

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


      };

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
