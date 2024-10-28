{ pkgs, ... }:
{
  # systemcli status nix-gc.service
  nix.gc = {
    automatic = true;
    dates = "03:05";
    options = "--delete-older-than 5d";
  };

  # systemcli status nix-optimise.service
  nix.optimise = {
    automatic = true;
    dates = [ "03:20" ];
  };
}
