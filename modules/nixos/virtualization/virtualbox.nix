{ hostSpecific, lib, config, ... }:
{
  config = lib.mkIf config.virtualisation.virtualbox.host.enable {

    virtualisation.virtualbox = {
      host = {
        # default virtualbox virtualisation module conflicts with kvm.
        # Further more there is no reason not to use kvm directly.
        enableKvm = true;
        # required when setting enableKvm = true;
        addNetworkInterface = false;

        enableExtensionPack = true;
      };
    };

    users.extraGroups.vboxusers.members = [ hostSpecific.primeUsername ];
    nixpkgs.config.allowUnfree = true;
  };
}
