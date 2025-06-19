{ config, lib, pkgs, ... }:

let
  cfg = config.customOptions.enableModule.nitrokey;
in
{
  options.customOptions.enableModule.nitrokey = lib.mkEnableOption "Enable Nitrokey support (CLI, GUI, udev rules)";

  config = lib.mkIf cfg {
    environment.systemPackages = with pkgs; [
      pynitrokey # cli for nk3
      nitrokey-app2 # gui for nk3

      # FIDO/FIDO(U2F) libraries dependencies.
      libfido2 # webauto
      pam_u2f # linux(sudo, display manager, console login...)
    ];

    services.udev.packages = [ pkgs.nitrokey-udev-rules ];
  };
}
