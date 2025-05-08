{
  programs.git = {

    extraConfig = {

      commit = {
        gpgSign = true;
      };

      gpg = {
        program = "gpg";
      };
    };
  };
}

