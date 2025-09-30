{ lib, ...}:
{
  imports = [
    ../../modules/nixos/custom-global-options/networking.nix
    ../../modules/nixos/custom-global-options/static-ip.nix
  ];

    options.customHostSpecificGlobalOptions = {
      pathToDataDirectory = lib.mkOption {
        default = "/data";
        type = lib.types.str;
        description = "Path to the data directory on the homelab host";
      };

      pathToMediaDirectory = lib.mkOption {
        default = "/media";
        type = lib.types.str;
        description = "Path to the media directory on the homelab host";
      };
  };
}
