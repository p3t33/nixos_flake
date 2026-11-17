{ config, lib, name, id, ... }:

let
  cfg = config.custom.services.syncthing.settings.devices.${name};
in
{
  options.custom.services.syncthing.settings.devices.${name}.enable = lib.mkEnableOption "Enable Syncthing device: ${name}";

  config = lib.mkIf cfg.enable {
    services.syncthing = {
      settings = {
        devices = {
          "${name}" = {
            inherit name id;
            autoAcceptFolders = false;
            introducer = false;
          };
        };
      };
    };
  };
}
