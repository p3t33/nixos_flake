{ config, lib, ... }:

{
  options.custom.hardware.cpuVendor = lib.mkOption {
    type = lib.types.enum [ "intel" "amd" ];
    default = "intel";
    description = "CPU vendor to control microcode updates.";
  };

  config = {
    hardware.cpu.${config.custom.hardware.cpuVendor}.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
