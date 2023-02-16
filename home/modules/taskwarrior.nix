{ ... }:
{
    programs.taskwarrior = {
        enable = true;

        # Will be used to sync data across devices  
        dataLocation = "$HOME/Sync/taskwarrior_data/task";

        config = {
            weekstart="Sunday";
            confirmation = true;

            alias."@" = "context";
            context."work" = "project:work";
            context."personal" = "project:personal";
        };
    };
}

