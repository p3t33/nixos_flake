{ ... }:
{

  programs.i3status = {
    enable = true;
    enableDefault = false;
    modules = {
      "volume master" = {
         position = 1;
         settings = {
         format = "♪ %volume";
         format_muted = "♪ muted (%volume)";
         device = "default";
         };
      };
      
      "disk /" = {
        position = 2;
        settings = {
          format = " %avail";
        };
      };

      "tztime local" = {
        position = 3;
        settings = {
          format = " %H:%M  %d-%m-%Y";
        };
      };

      "battery 0" = {
        position = 4;
        settings = {
          format = "  %percentage ";
          format_down = "No battery";
          integer_battery_capacity = true;
          last_full_capacity = true;
        };
      };
    };
 };

}
