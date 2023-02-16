{ config, ... }:

{

  programs.git = {
    enable = true;
    delta = {
      enable = true;
      options = {
        navigate = true;
        line-numbers = true;
        side-by-side = true;
        theme = "Nord";
      };
    };

    userName = config.userDefinedGlobalVariables.gitUser;
    userEmail = config.userDefinedGlobalVariables.email;

    aliases = {
        cp = "cherry-pick";
        st = "status";
        cm = "commit";
        cma = "commit --amend";

        # add all uncommitted and un-staged changes currently in the working 
        # directory and add them to the previous commit, stopping for amending 
        caa = "commit -a --amend -C HEAD";
        co = "checkout";
        br = "branch";

        rl = "remote -v";

    };

   };  

   # will create ~/.config/git/ingore file
   # This file will be used as a global ignore file for git
   xdg.configFile."git/ignore".text = ''
    .vim-bookmarks
    tags
  '';
  
}
