{ pkgs, ... }:
{
    # This clipbpard history daemon depends on xclip and might
    # not work with wayland.
    services.clipmenu = {
        enable = true;
        launcher = "rofi";
    };
}

