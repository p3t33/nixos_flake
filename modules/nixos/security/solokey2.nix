{ config, lib, pkgs, ... }:

let
  cfg = config.custom.security.solokey2;
in
{
  options.custom.security.solokey2.enable = lib.mkEnableOption "Enable Solo 2 security key support";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # FIDO/FIDO(U2F) libraries dependencies.
      libfido2 # webauto
      pam_u2f # linux(sudo, display manager, console login...)
      solo2-cli
    ];

    # allows to execute "solo2 update" command without sudo.
    services.udev.packages = [
      (pkgs.writeTextFile {
        name = "wally_udev";
        text = ''
          # NXP LPC55 ROM bootloader (unmodified)
          SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1fc9", ATTRS{idProduct}=="0021", TAG+="uaccess"
          # NXP LPC55 ROM bootloader (with Solo 2 VID:PID)
          SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="b000", TAG+="uaccess"
          # Solo 2
          SUBSYSTEM=="tty", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="beee", TAG+="uaccess"
          # Solo 2
          SUBSYSTEM=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="beee", TAG+="uaccess"
        '';
        destination = "/etc/udev/rules.d/70-solo2.rules";
      })
    ];
  };
}
