{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    pynitrokey # cli for nk3
    nitrokey-app2 # gui for nk3

    # FIDO/FIDO(U2F) libraries dependencies.
    libfido2 # webauto
    pam_u2f # linux(sudo, display manager, console login...)
  ];

  services.udev.packages = [ pkgs.nitrokey-udev-rules ];
}
