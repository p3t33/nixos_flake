{ config, lib, ... }:

{
  options.customOptions.cpuVendor = lib.mkOption {
    type = lib.types.enum [ "intel" "amd" ];
    default = "intel";
    description = "CPU vendor to control microcode updates.";
  };

  config = {
    hardware.cpu.${config.customOptions.cpuVendor}.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
