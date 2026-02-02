{ config, lib, ... }:
{

  config = lib.mkIf config.programs.ghostty.enable {
    programs.ghostty = {
      installBatSyntax = true;
      installVimSyntax = true;
      
      # Enable systemd integration for better performance and features
      # See: https://ghostty.org/docs/linux/systemd
      systemd.enable = true;
      
      settings = {
        font-size = 16;
        window-decoration = "server";

        mouse-hide-while-typing = true;

        mouse-scroll-multiplier = 2; # Adjust to your preference
        scrollback-limit = 100000;   # Increase scrollback buffer size


        background-opacity = 0.90; # Adjust for desired transparency

        copy-on-select = "clipboard";
        app-notifications = "no-clipboard-copy";

        # to list all avaliable themes(names might change like nord -> Nord).
        #
        # ghostty +list-themes 2>&1
        theme = "Nord";
      };
    };
  };
}

