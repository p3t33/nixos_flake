{ ... }:

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

    userName = "Kobi Medrish";
    userEmail = "kobi.medrish@gmail.com";
    };  
  
}
