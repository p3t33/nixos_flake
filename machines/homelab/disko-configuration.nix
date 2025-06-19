# uefi/bios hybrid support.
# git clone https://github.com/p3t33/nixos_flake
# sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko ./nixos_flake/hosts/homelab/disko-config.nix
# sudo nixos-install --flake ./nixos_flake#homelab
{
  disko.devices = {
    disk = {
      sda = {
        type = "disk";
        device = "/dev/sda";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              size = "1M";
              type = "EF02"; # for grub MBR
            };
            ESP = {
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
