{ config, lib, pkgs, pkgs-unstable, ... }:

let
  palette = config.custom.shared.omarchyColors or {
    background = "#1a1b26";
    bright_foreground = "#c0caf5";
    accent = "#7aa2f7";
    dark_foreground = "#565f89";
    selection = "#292e42";
    lighter_background = "#24283b";
    magenta = "#ad8ee6";
    yellow = "#e0af68";
    red = "#f7768e";
  };
in
{

  config = lib.mkIf config.programs.lazygit.enable {
    programs.lazygit = {
      # Stable lazygit lacks Alt+letter keybinding support.
      package = pkgs-unstable.lazygit;
      settings = {
        gui.theme = {
          activeBorderColor = [ palette.accent "bold" ];
          inactiveBorderColor = [ palette.dark_foreground ];
          searchingActiveBorderColor = [ palette.accent "bold" ];
          optionsTextColor = [ palette.accent ];
          selectedLineBgColor = [ palette.selection ];
          inactiveViewSelectedLineBgColor = [ palette.lighter_background ];
          cherryPickedCommitFgColor = [ palette.background ];
          cherryPickedCommitBgColor = [ palette.magenta ];
          markedBaseCommitFgColor = [ palette.background ];
          markedBaseCommitBgColor = [ palette.yellow ];
          unstagedChangesColor = [ palette.red ];
          defaultFgColor = [ palette.bright_foreground ];
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
