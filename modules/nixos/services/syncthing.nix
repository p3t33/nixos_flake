{ config, ... }:
{
    services.syncthing = {
        enable = true;
        group = "data";
    };
}
