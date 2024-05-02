{  pkgs, ... }:
{
     boot.initrd.kernelModules = [ "amdgpu" ];

     environment.systemPackages = with pkgs; [
         mesa.drivers # For Vulkan support
         rocm-opencl-icd # For OpenCL support
     ];

     services.xserver = {
         enable = true;
         videoDrivers = [ "amdgpu" ]; # Enables the AMDGPU driver
     };
}
