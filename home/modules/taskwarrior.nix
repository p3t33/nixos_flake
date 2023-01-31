{ ... }:
{
    programs.taskwarrior = {
        enable = true;

        # Will be used to sync data across devices  
        dataLocation = "$HOME/Sync/taskwarrior_data/task";
    };
}

