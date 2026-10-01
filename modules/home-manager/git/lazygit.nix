{ config, lib, pkgs, pkgs-unstable, ... }:

{

  config = lib.mkIf config.programs.lazygit.enable {
    programs.lazygit = {
      # Stable lazygit lacks Alt+letter keybinding support.
      package = pkgs-unstable.lazygit;
      settings = {
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
