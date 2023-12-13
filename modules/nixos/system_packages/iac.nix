{ config, pkgs, ... }:
{
    environment.systemPackages = with pkgs; [

      # infrastructure as code
      packer
      vagrant
      ansible
    ];
}



