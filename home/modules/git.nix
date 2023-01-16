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
        kobi = "log";

    };

   };  

   # will create ~/.config/git/ingore file
   # This file will be used as a global ignore file for git
   xdg.configFile."git/ignore".text = ''
    .vim-bookmarks
  '';
  
}
