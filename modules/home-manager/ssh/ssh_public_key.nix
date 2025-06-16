{ config, lib, ... }:
{
  options.customOptions = {
      sshPublicKey = lib.mkOption {
      default = "";
      type = lib.types.str;
      description = "The name of the wallpaper file";
    };
  };

  config = {
    home.file = {
      ".ssh/smartcard.pub".text = ''
        ${config.customOptions.sshPublicKey}
      '';
    };
  };
}
