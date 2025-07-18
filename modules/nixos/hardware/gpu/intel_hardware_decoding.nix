{ config, pkgs, lib, ... }:

let
  cfg = config.customOptions.enableModule.intelHardwareDecoding;
in
{
  options.customOptions.enableModule.intelHardwareDecoding = lib.mkEnableOption "Enable Intel VA-API/VDPAU hardware video decoding";

  config = lib.mkIf cfg {
    nixpkgs.config.packageOverrides = pkgs: {
      intel-vaapi-driver = pkgs.intel-vaapi-driver.override { enableHybridCodec = true; };
    };

    environment.systemPackages = with pkgs; [
      libva-utils # vainfo command to test the state of VA-API.
    ];

    # To test you can use vainfo
    # adding the packages into envionment.systemPackages did not work for me.
    hardware.graphics = {
      extraPackages = with pkgs; [
        intel-media-driver # LIBVA_DRIVER_NAME=iHD
        intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
        libvdpau-va-gl
      ];
    };

    environment.sessionVariables = {
      LIBVA_DRIVER_NAME = "iHD";
    }; # Force intel-media-driver
  };
}
