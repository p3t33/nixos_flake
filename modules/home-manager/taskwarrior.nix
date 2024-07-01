{ pkgs, ... }:
{
    programs.taskwarrior = {
        enable = true;
        package = pkgs.taskwarrior3;

        # Will be used to sync data across devices
        dataLocation = "$HOME/Sync/taskwarrior_data/task";

        config = {
            weekstart="Sunday";
            confirmation = true;
            dateformat = "D-M-Y";
            news.version="3.0.2"; # this settings supress the random news message.

            alias."@" = "context";
            context."work" = "project:work";
            context."personal" = "project:personal";
        };
    };
}

