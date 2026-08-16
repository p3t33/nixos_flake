{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.services.gnome.gnome-keyring.enable {
    services.gnome.gcr-ssh-agent.enable = false;
    environment.systemPackages = [ pkgs.seahorse ];
  };
}
