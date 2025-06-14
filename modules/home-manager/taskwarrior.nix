{ pkgs, hostSpecification, ... }:
{
  home.packages = with pkgs; [ taskwarrior-tui ];
  programs.taskwarrior = {
    enable = true;
    package = pkgs.taskwarrior3;

    # Will be used to sync data across devices
    dataLocation = "${hostSpecification.syncthing.syncDir}/taskwarrior_data/task";

    config = {
      weekstart = "Sunday";
      confirmation = true;
      dateformat = "Y-M-D H:N";
      news.version = "3.1.0"; # this settings supress the random news message.

      alias."@" = "context";
      context."work" = "project:work";
      context."personal" = "project:personal";
      color = {
        active = "rgb450";
        tag = {
          next = "white";
          none = "white";
          tagged = "white";
        };
        blocked = "white";
        blocking = "white";
      };
    };
  };
}
