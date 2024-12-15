{ pkgs, config, ... }:
{
  virtualisation.libvirtd = {
    enable = true;
    # requried for shared folders with virtiofs
    qemu.vhostUserPackages = with pkgs; [ virtiofsd ];
  };

  virtualisation.spiceUSBRedirection.enable = true;
  users.users.${config.userDefinedGlobalVariables.primeUsername} = {
    extraGroups = [ "libvirtd" ];
  };

  environment.systemPackages = with pkgs; [
    virt-manager
    qemu_kvm
    qemu
    libvirt
  ];
}
