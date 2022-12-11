{ pkgs, ...}:
{

  services.udev.packages = [ pkgs.moolticute.udev ];

}
