{ ... }:
{
  services.syncthing.enable = true;

  # Only intended for the use of NixOS that runs as a VM(guest) and has a GUI.
  services.qemuGuest.enable = true;
  services.spice-vdagentd.enable = true;

  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };

    vpn.wireguardQuickClient.enable = true;
  };
}
