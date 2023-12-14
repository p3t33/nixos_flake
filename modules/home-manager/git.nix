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

    # User and email defined in this file that is generated using sops
    # For some reason git config --global user.[name/email] shows empty but
    # executing git config user.[name/email] inside of a repo works just fine.
    extraConfig = {
    include = {
      path = "${config.userDefinedGlobalVariables.homeDirectory}/.config/git/credentials";
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

   # will create ~/.config/git/ingore file
   # This file will be used as a global ignore file for git
   xdg.configFile."git/ignore".text = ''
    .vim-bookmarks
    Session.vim
    tags
    .clang-format
  '';

}
