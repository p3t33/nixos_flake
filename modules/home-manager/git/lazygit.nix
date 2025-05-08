{
  programs.lazygit = {
    enable = true;
    settings = {
      promptToReturnFromSubprocess = false;
      git = {
        paging = {
          colorArg = "always";
          useConfig = false;

          # By default, tools like git use a pager to display
          # their output, so you can comfortably read through long diffs
          # delta too uses paging. In the context of integrating delta
          # with lazygit it is useful to use the --paging=never switch
          # to prevent netsted paging and comflicts between the too.
          pager = "delta --paging=never";

          diffPagingArg = " ";
        };
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
}
