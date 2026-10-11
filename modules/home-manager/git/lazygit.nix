{ config, lib, pkgs, pkgs-unstable, ... }:

let
  colors = {
    background = "#2e3440";
    bright_foreground = "#d8dee9";
    accent = config.custom.shared.outlineColors.active or "#88c0d0";
    dark_foreground = "#4c566a";
    selection = "#434c5e";
    lighter_background = "#3b4252";
    magenta = "#b48ead";
    yellow = "#ebcb8b";
    red = "#bf616a";
  };
in
{

  config = lib.mkIf config.programs.lazygit.enable {
    programs.lazygit = {
      # Stable lazygit lacks Alt+letter keybinding support.
      package = pkgs-unstable.lazygit;
      settings = {
        gui.theme = {
          activeBorderColor = [ colors.accent "bold" ];
          inactiveBorderColor = [ colors.dark_foreground ];
          searchingActiveBorderColor = [ colors.accent "bold" ];
          optionsTextColor = [ colors.accent ];
          selectedLineBgColor = [ colors.selection ];
          inactiveViewSelectedLineBgColor = [ colors.lighter_background ];
          cherryPickedCommitFgColor = [ colors.background ];
          cherryPickedCommitBgColor = [ colors.magenta ];
          markedBaseCommitFgColor = [ colors.background ];
          markedBaseCommitBgColor = [ colors.yellow ];
          unstagedChangesColor = [ colors.red ];
          defaultFgColor = [ colors.bright_foreground ];
        };
        promptToReturnFromSubprocess = false;
        git = {
          diffRenderers =
          [
            {
              # By default, tools like git use a pager to display
              # their output, so you can comfortably read through long diffs
              # delta too uses paging. In the context of integrating delta
              # with lazygit it is useful to use the --paging=never switch
              # to prevent netsted paging and comflicts between the too.
              command = "delta --dark --paging=never";

            }
            {
                type = "extDiff";
                command = "${lib.getExe pkgs.difftastic} --color=always";
            }
          ];
        };

        keybinding = {
          commits = {
            moveUpCommit = "<c-k>"; # only works outside of tmux.
            moveDownCommit = "<c-j>"; # only works outside of tmux.
          };

          universal = {
            scrollUpMain = "<alt+k>";
            scrollDownMain = "<alt+j>";
          };
        };
      };
    };
  };
}
