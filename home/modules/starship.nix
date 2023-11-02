{ lib, ... }:

{

programs.starship = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
      settings = {

        command_timeout = 2000; # in milliseconds
        # new line above the prompt.
        add_newline = false;
        scan_timeout = 10;

        # Order of right side modules of the prompt
        #
        # Creating elements that blend
        # ----------------------------
        # The idia is to set a backgroung for an element
        # then use the triangle which will have the foreground of the
        # previus elemnt and the background of the following elment.
        # and put the triangle as an elemnt in the order, for example
        # "directory"
        # "[](fg:#3B76F0 bg:#FCF392)"
        # "git_branch"
        # with directory background being #3B76F0 and the git_branch background
        # being #FCF392
        #
        # Another cool trick is to creat rounded corners using the same principle
        # for example: "[](bg:#030B16 fg:#7DF9AA)" that you put in the begining of the
        # format order here is a commentedf out more detalied example:
        # format = lib.concatStrings [
        #    "[](bg:#030B16 fg:#7DF9AA)"
        #    "$directory"
        #    "$c"
        #    "[](fg:#3B76F0 bg:#FCF392)"
        #    "$git_branch"
        #    "$git_status"
        #    "[](fg:#FCF392 bg:#030B16)"
        #    "$fill" # This will push the right prompt to the right edge
        #    "$cmd_duration"
        #    "$time"
        #    "$status"
        #    "$line_break"
        #    "$character"
        #];
        format = lib.concatStrings [
            "$hostname"
            "$directory"
            "$python"
            "$c"
            "$git_branch"
            "$git_status"
            "$fill" # This will push the right prompt to the right edge
            "$cmd_duration"
            "$time"
            "$status"
            "$line_break"
            "$character"
        ];

        # Only used when setting a single left side prompt, otherwise fill moudle
        # should be used.
        #
        # Order of right side modules of the prompt
        right_format = lib.concatStrings [
        ];

        # Used to align/create "left and right prompt on the same line when two
        # line prompt is is used.
        fill = {
            symbol = " ";
            style = "bold green";
        };


        time = {
            disabled = false;
            time_format = "%R";
            style = "bg:#1d2230";
            format = "[ 󱑍 $time]($style)";
        };

        # creates the two line prompt.
        line_break = {
            disabled = false;
        };

        directory = {
          read_only = "🔒"; # icon with some blank space
          truncation_length = 10;
          truncation_symbol = "";
          truncate_to_repo = true;

          format = "[ [$read_only]($read_only_style) $path]($style)";
          read_only_style = "red";
          style = "fg:#00B0FF";
        };


        git_branch = {
            symbol = "  ";
            style = "fg:#5FD700";
            format = "[$symbol$branch(:$remote_branch)]($style)";
        };

        # Might be slow on windows.
        git_status = {
            format = "[ $all_status$ahead_behind]($style)";
            style = "fg:#5FD700";
        };



        cmd_duration = {
            format = "[ took 󱦟 $duration at]($style)";
            style = "bg:#1d2230";
            min_time = 500;
        };

        character = {
            success_symbol = "[ ➜](#5FD700)";
            error_symbol = "[ ✗](#E84D44)";
            vimcmd_symbol = "[ ❮](bold green)";
            vimcmd_replace_one_symbol = "[ ❮](bold purple)";
            vimcmd_replace_symbol =  "[ ❮](bold purple)";
            vimcmd_visual_symbol =  "[ ❮](bold yellow)";
            disabled = false;
        };

        hostname = {
            ssh_only = true;
            format = "[$hostname $ssh_symbol]($style)";
            ssh_symbol = "";
            style = "bold dimmed green";
            disabled = false;
        };

        golang = {
            symbol = " ";
        };

        rust = {
            symbol = " ";
        };

        c = {
            symbol = " ";
            format = "[ $symbol($version(-$name) )]($style)";
        };

        nix_shell = {
            symbol = " ";
        };

        package = {
            symbol = " ";

        };

        python = {
            symbol = " ";
            format = "[ $symbol($version(-$name) )]($style)";
        };

        os = {
            format = "[ $symbol]($style)";
            style = "fg:#D8DEE9";
            symbols = {
                Ubuntu = " ";
            };
            disabled = false;
        };


        # error status of executed command.
        status = {
            style = "fg:#FD0000";
            format = "[ ✗ $int - $common_meaning]($style) ";
            map_symbol = true;
            disabled = false;
        };

        };
      };
}
