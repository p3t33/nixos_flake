{ pkgs, ... }:
{
  boot.initrd.kernelModules = [ "amdgpu" ];

  environment.systemPackages = with pkgs; [
    mesa.drivers # For Vulkan support
    rocm-opencl-icd # For OpenCL support

    # dependencies to provid use space apps hardware acceleration
    libva # (libarary for video acceleraion), its the core library for VA-API.
    libva-utils # has vainfo that can be used get information about VA-API state.

  ];

  # dependencies to provid use space apps hardware acceleration
  environment.variables = {
    LIBVA_DRIVER_NAME = "radeonsi";
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "amdgpu" ]; # Enables the AMDGPU driver
  };
}
