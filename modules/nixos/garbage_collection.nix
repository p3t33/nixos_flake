{ pkgs,  ... }:
{
    nix.gc = {
        automatic = true;
        dates = "03:05";
        options = "--delete-older-than 30d";
    };
}
