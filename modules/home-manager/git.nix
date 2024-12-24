{ config, ... }:

{

  programs.git = {
    enable = true;
    delta = {
      enable = true;
      options = {
        navigate = true;
        line-numbers = true;
        side-by-side = false;
        theme = "Nord";
      };
    };

    # will create ~/.config/git/ingore file
    # This file will be used as a global ignore file for git
    ignores = [
      ".vim-bookmarks"
      "Session.vim"
      "tags"
      ".clang-format"
      ".ccls-cache"
    ];

    extraConfig = {

      core = {
        editor = "${config.userDefinedGlobalVariables.editor}";
      };

      pull = {
        # This is the default for branches that has no expicit confiugraions,
        # like the default one I set with autosetuprebase.
        rebase = true;
      };

      push = {
        # This directive makes git to apply force only if the local branch that
        # is tracking the remote(E.g origin/master) hasn"t diverged from the remote"
        # meaning in case that no new commits were added to the remote master.
        # It avoides overwriting someone else work on the remote.
        #
        # implicitly makes "git push -f" into "git push --force-with-leas"
        # to explicitly use "git pushh -f" you will need to execute "git push --force".
        forceWithLease = true;
      };

      branch = {
        # This directive will rebase local chagnes on top of the remote branch
        # in case branches diverged instead of the defualt of creating a commit
        # on the local branch which will diverge from the remote branch.
        # using rebase is a cleaner solution and will keep the history linear.
        #
        # This will only be applied on branches that are created after this
        # config was added.
        autosetuprebase = "always";

        # when creating a new branch(foo), will automatically linking it
        # to thier correspondig remote branches(E.g origin/foo) if they exist
        # on the remote so you can immediately use commands like git pull/push
        # wihtout any additional setup.
        autosetupmerge = "always";
      };

      merge = {
        conflictStyle = "zdiff3";
      };
      include = {
        # User and email defined in this file that is generated using sops
        # For some reason git config --global user.[name/email] shows empty but
        # executing git config user.[name/email] inside of a repo works just fine.
        path = config.sops.secrets.git.path;
      };
    };

    aliases = {
      cp = "cherry-pick";
      st = "status";
      cma = "commit --amend";

      # add all uncommitted and un-staged changes currently in the working
      # directory and add them to the previous commit, stopping for amending
      caa = "commit -a --amend -C HEAD";
      co = "checkout";
      br = "branch";
      cc = "commit";
      cm = "commit -m";
      amend = "commit --amend";
      ls = "log --stat";
      aa = "add --all";

      rem = "remote -v";
      rebi = "rebase -i";

      pl = "pull";
      ph = "push";

    };

  };

}
