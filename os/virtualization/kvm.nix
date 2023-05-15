{ pkgs, config, ... }:
{
    virtualisation.libvirtd.enable = true;
    users.users.${config.userDefinedGlobalVariables.username} = {
        extraGroups = [ "libvirtd" ];
    };

    environment.systemPackages = with pkgs; [
        virt-manager
        qemu_kvm
        qemu
        libvirt
    ];
}
