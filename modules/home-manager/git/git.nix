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
      # java build files
      "*.class"
      # Eclipse-specific files
      ".classpath"
      ".project"
      ".settings/"
      "*.launch"
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

        # Will only push the current branch if it is tracking a remote branch (e.g., origin/foo).
        # If the current branch has no upstream set, `git push` will fail.
        # This prevents accidentally pushing branches that are not explicitly linked to a remote.
        default = "simple";

        # Automatically sets an upstream remote when pushing a new local branch for the first time.
        #
        # Example: git checkout -b foo && git push
        #
        # This eliminates the need to manually run 'git push -u origin foo'
        # by automatically linking 'foo' to 'origin/foo' upon first push.
        #
        # Unlike autosetupmerge, which only applies when checking out an existing remote branch
        # (allowing 'git pull' to work immediately), this setting ensures that a completely
        # new branch, one that does NOT yet exist on the remote that is correctly tracked after pushing.
        #
        # This means that future 'git pull' and 'git push' will work seamlessly without extra setup.
        autoSetupRemote = true;

        # Pushes only annotated tags related to the commits being pushed.
        followTags = true;
      };

      rebase = {
         # Automatically stashes changes that are not committed before a rebase, and restores them after the rebase completes.
         autoStash = true;
      };

      fetch = {
        # Automatically removes remote-tracking branches (origin/*) that no longer exist on the remote.
        prune = true;

        # Removes local tags that no longer exist on the remote when fetching.
        pruneTags = true;
        # Ensures git fetch always fetches from all remotes, not just origin, in case you have more then one.
        all = true;
      };

      help = {
        # tells Git to automatically suggest and (after a brief delay)
        # correct a mistyped command
        autocorrect = "prompt";
      };

      column =
      {
        # how git displays lists(E,g branches), with this config, git will use
        # column based on termnial space.
        ui = "auto";
      };


      commit = {
        # when commit created wihtout -m "..." editor will have the changes inside the
        # commit message.
        verbose = true;
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

        # Automatically sets a remote branch to follow when checking out an existing remote branch.
        #
        # Example: git checkout -b dev origin/dev
        #
        # This allows 'git pull' to work immediately without needing --set-upstream.
        # Since the branch is created from origin/dev, it is NOT a new branch in the repository.
        # However, this does NOT affect 'git push' for completely new branches created with
        #
        # git checkout -b foo
        #
        # which are coverd with autoSetupRemote = true
        autosetupmerge = "always";


        # sort branches by most recent commit, in descending order.
        sort = "-committerdate";
      };

      tag = {
        sort = "version:refname";
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
