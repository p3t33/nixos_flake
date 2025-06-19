{ config, lib, ... }:
let
    cfg = config.custom.file.smartcardPublicKey;
in
{
  options.custom.file.smartcardPublicKey = {
    enable = lib.mkEnableOption "Enable writing the smartcard SSH public key to ~/.ssh/smartcard.pub";
    value = lib.mkOption {
      default = "";
      type = lib.types.str;
      description = "The name of the wallpaper file";
    };
  };

  config = lib.mkIf cfg.enable {
    home.file = {
      ".ssh/smartcard.pub".text = ''
        ${cfg.value}
      '';
    };
  };
}
