{ config, lib, ... }:
{

  config = lib.mkIf config.programs.ghostty.enable {
    programs.ghostty = {
      installBatSyntax = true;
      installVimSyntax = true;
      settings = {
        font-size = 16;
        window-decoration = "server";

        mouse-hide-while-typing = true;

        mouse-scroll-multiplier = 2; # Adjust to your preference
        scrollback-limit = 100000;   # Increase scrollback buffer size


        background-opacity = 0.90; # Adjust for desired transparency

        copy-on-select = "clipboard";
        app-notifications = "no-clipboard-copy";

        theme = "nord";
      };
    };
  };
}

