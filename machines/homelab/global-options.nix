{ lib, ...}:
{
  imports = [
    ../../modules/nixos/custom-global-ptions/networking.nix
    ../../modules/nixos/custom-global-ptions/static-ip.nix
  ];

  options.customHostSpecificGlobalOptions = {
    pathToDataDirectory = lib.mkOption {
      default = "/mnt/data";
      type = lib.types.str;
      description = "Path to the data directory on the homelab host";
    };

    pathToMediaDirectory = lib.mkOption {
      default = "/mnt/media";
      type = lib.types.str;
      description = "Path to the media directory on the homelab host";
    };
  };
}
