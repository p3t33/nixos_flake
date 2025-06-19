{ config, lib, ... }:
let
    cfg = config.customOptions.enableModule.enableSmartcardPublicKey;
in
{
  options.customOptions.enableModule.enableSmartcardPublicKey = lib.mkEnableOption "Enable writing the smartcard SSH public key to ~/.ssh/smartcard.pub";

  options.customOptions = {
      sshPublicKey = lib.mkOption {
      default = "";
      type = lib.types.str;
      description = "The name of the wallpaper file";
    };
  };

  config = lib.mkIf cfg {
    home.file = {
      ".ssh/smartcard.pub".text = ''
        ${config.customOptions.sshPublicKey}
      '';
    };
  };
}
