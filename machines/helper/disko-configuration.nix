# uefi only
{ ... }:
let
  pools = { rootPool = "rpool"; };
  disks = {
    os = {
      name = "os";
      path = "/dev/disk/by-id/nvme-PM991_NVMe_Samsung_512GB__S509NF0N884037";
    };
  };
in
{
  disko.devices = {
    disk = {
      "${disks.os.name}" = {
        type = "disk";
        device = disks.os.path;
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
                mountOptions = [ "defaults" ];
              };
            };
            swap = {
              size = "4G";
              content = {
                type = "swap";
                discardPolicy = "both";
              };
            };
            root = {
              size = "100%";
              content = {
                type = "zfs";
                pool = pools.rootPool;
              };
            };
          };
        };
      };
    };
    zpool = {
      ${pools.rootPool} = {
        type = "zpool";
        options = {
          ashift = "12";
        };
        rootFsOptions = {
          mountpoint = "none";
          acltype = "posixacl";
          xattr = "sa";
          atime = "off";
          compression = "lz4";
        };
        datasets = {
          "ROOT" = { type = "zfs_fs"; options.mountpoint = "none"; };

          "ROOT/nixos" = {
            type = "zfs_fs";
            options.mountpoint = "legacy";
            mountpoint = "/";
          };

          "home" = {
            type = "zfs_fs";
            options.mountpoint = "legacy";
            mountpoint = "/home";
          };

          "nix" = {
            type = "zfs_fs";
            options.mountpoint = "legacy";
            options.compression = "lz4";
            mountpoint = "/nix";
          };

          "tmp" = {
            type = "zfs_fs";
            options.mountpoint = "legacy";
            options."com.sun:auto-snapshot" = "false";
            mountpoint = "/tmp";
          };

          "var_log" = {
            type = "zfs_fs";
            options.mountpoint = "legacy";
            mountpoint = "/var/log";
          };

          "reserved" = {
            type = "zfs_fs";
            options.mountpoint = "none";
            options.refreservation = "2G";
            options."com.sun:auto-snapshot" = "false";
          };
        };
      };
    };
  };
}
