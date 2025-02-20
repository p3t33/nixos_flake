{ config, ... }:
{
  # As I am using nvidia proproitnery drivers
  # I don't need to install mesa.drivers and rocm-opencl-icd
  # as they are already part of the driver.

  services.xserver = {
    enable = true;
    videoDrivers = [ "modesetting" "nvidia" ];
  };

  hardware.nvidia = {
    modesetting.enable = true;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # Note there is also an option to add the driver to load early
  # but this is duggested only as part of Troubleshooting(unlike in the case
  # of amd).
}
