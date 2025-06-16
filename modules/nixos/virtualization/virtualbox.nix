{ hostSpecific, ... }:
{
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  virtualisation.virtualbox.guest.enable = true;
  users.extraGroups.vboxusers.members = [ hostSpecific.primeUsername ];
}
