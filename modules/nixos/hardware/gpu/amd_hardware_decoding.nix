{ config, pkgs, lib, ... }:

let
  cfg = config.customOptions.enableModule.amdHardwareDecoding;
in
{
  options.customOptions.enableModule.amdHardwareDecoding = lib.mkEnableOption "Enable AMD VA-API decoding (libva, radeonsi)";

  config = lib.mkIf cfg {
    hardware.graphics = {
      extraPackages = with pkgs; [
        # dependencies to provid use space apps hardware acceleration
        libva # (libarary for video acceleraion), its the core library for VA-API.
        libva-utils # has vainfo that can be used get information about VA-API state.
      ];
    };

    # dependencies to provid user space apps hardware acceleration
    environment.variables = {
      LIBVA_DRIVER_NAME = "radeonsi";
    };
  };
}
