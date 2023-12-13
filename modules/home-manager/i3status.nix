{ ... }:
{

  programs.i3status = {
    enable = true;
    # Enables display of defult data
    # can work along with set modules
    enableDefault = false;


    # According to documentation the format for
    # setting the modules option is as follows:
    #
    # programs.i3status.modules.<name>.<option>
    # so for example "volume master" is a "name"
    #
    # by default each "name" that is defined is
    # enabled implicitly. 
    # programs.i3status.modules.<name>.enable
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
