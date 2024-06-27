{ lib, pkgs, ... }:
let
  i3-monitor = pkgs.writeShellScriptBin "i3-monitor" ''
    builtin_display="eDP-1" # Replace with your actual built-in display identifier

    # Get the list of connected monitors except the built-in one
    external_monitors=$(xrandr | grep " connected" | grep -v "$builtin_display" | cut -d " " -f1)

    # Check if any external monitors are connected
    if [ -n "$external_monitors" ]; then
    # If external monitors are connected, disable the built-in display and enable external ones
        xrandr --output "$builtin_display" --off
        for monitor in $external_monitors; do
        xrandr --output "$monitor" --auto
    done
    else
    # If no external monitors are connected, enable the built-in display
        xrandr --output "$builtin_display" --auto
    fi

  '';
in
{
    home.packages = [ i3-monitor ];
}

