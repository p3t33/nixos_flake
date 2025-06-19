{ config, pkgs, lib, ... }:

let
  cfg = config.custom.hardware.amd;
in
{
  options.custom.hardware.amd.enable = lib.mkEnableOption "Enable AMD GPU support with VA-API and Vulkan acceleration";

  config = lib.mkIf cfg.enable {
    boot.initrd.kernelModules = [ "amdgpu" ];

    environment.systemPackages = with pkgs; [
      mesa # For Vulkan support
      mesa.opencl

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
  };
}
