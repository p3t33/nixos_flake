{ ... }:

{
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;

    flags = [ "--disable-up-arrow" ];

    settings = {

      style = "compact";
      # for some reason this directive has no effect.
      show_help = false;
    };
  };
}
