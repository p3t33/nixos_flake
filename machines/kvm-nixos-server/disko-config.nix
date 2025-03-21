# uefi only which means that the VM needs to be set with uefi boot
{ disks ? [ "/dev/vda" ]
, zpoolName ? "rpool"
, ...
}:

{
  disko.devices = {
    disk = {
      first = {
        type = "disk";
        device = builtins.elemAt disks 0;
        content = {
          type = "gpt";
          partitions = {
            esp = {
              type = "EF00";
              size = "1G";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [
                  "defaults"
                ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "zfs";
                pool = zpoolName;
              };
            };
          };
        };
      };
    };
    zpool = {
      "${zpoolName}" = {
        type = "zpool";
        options = {
          ashift = "12";
        };
        rootFsOptions = {
          mountpoint = "none";
          acltype = "posixacl";
          xattr = "sa";
          atime = "off";
          encryption = "on";
          keyformat = "passphrase";
        };
        datasets = {
          "safe" = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          "safe/root" = {
            type = "zfs_fs";
            options.mountpoint = "legacy";
            mountpoint = "/";
          };
          "local" = {
            type = "zfs_fs";
            options.mountpoint = "none";
          };
          "local/reserved" = {
            type = "zfs_fs";
            options.mountpoint = "none";
            options.refreservation = "2G";
          };
          "local/nix" = {
            type = "zfs_fs";
            options.mountpoint = "legacy";
            options.compression = "lz4";
            mountpoint = "/nix";
          };
          "local/tmp" = {
            type = "zfs_fs";
            options.mountpoint = "legacy";
            mountpoint = "/tmp";
          };
        };
      };
    };
  };
}
