{ config, lib, ... }:

{

  config = lib.mkIf config.programs.lazygit.enable {
    programs.lazygit = {
      settings = {
        promptToReturnFromSubprocess = false;
        git = {
          pagers =
          [
            {
              # By default, tools like git use a pager to display
              # their output, so you can comfortably read through long diffs
              # delta too uses paging. In the context of integrating delta
              # with lazygit it is useful to use the --paging=never switch
              # to prevent netsted paging and comflicts between the too.
              pager = "delta --dark --paging=never";

            }
            {
                externalDiffCommand = "difft --color=always";
            }
          ];
        };

        keybinding = {
          commits = {
            moveUpCommit = "<c-k>"; # only works outside of tmux.
            moveDownCommit = "<c-j>"; # only works outside of tmux.
          };

          universal = {
            scrollUpMain-alt1 = "K";
            scrollDownMain-alt1 = "J";
          };
        };
      };
    };
  };
}
