{ ... }:

{

programs.starship = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
      settings = {

        add_newline = false;
        username = {
          style_user = "green bold";
          style_root = "red bold";
          format = "[$user]($style)";
          disabled = false;
          show_always = true;
        };

        hostname = {
          ssh_only = false;
          format = "@[$hostname](bold yellow):";
          trim_at = ".";
          disabled = false;
        };
        character = {
          success_symbol = "[❯](bold green)";
          error_symbol = "[✗](bold red)";
        };

        directory = {
          read_only = " ";
          truncation_length = 10;
          truncate_to_repo = true;
          style = "bold italic blue";
        };

        cmd_duration = {
          min_time = 2;
          show_milliseconds = false;
          disabled = false;
          style = "bold italic red";
        };

        aws = {
          symbol = "  ";
        };

        git_branch = {
          symbol = " ";
        };

        golang = {
          symbol = " ";
        };

        rust = {
          symbol = " ";
        };

        c = {
          symbol = " ";
        };

        nix_shell = {
          symbol = " ";
        };

        package = {
          symbol = " ";

        };

        python = {
          symbol = " ";
        };


      };
    };
}
