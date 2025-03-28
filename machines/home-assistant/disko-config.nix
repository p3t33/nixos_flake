# uefi only which means that the VM needs to be set with uefi boot
{ ... }:
let
  rootPool = "rpool";
in
{
  disko.devices = {
    disk = {
      first = {
        type = "disk";
        device = "/dev/sda";
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
                pool = rootPool;
              };
            };
          };
        };
      };
    };
    zpool = {
      ${rootPool} = {
        type = "zpool";
        options = {
          ashift = "12";
        };
        rootFsOptions = {
          mountpoint = "none";
          acltype = "posixacl";
          xattr = "sa";
          atime = "off";
          compression = "lz4"; # zstd is better but more resource demanding.
          # encryption = "on";
          # keyformat = "passphrase";
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
          "snapshots" = {
              type = "zfs_fs";
              options.mountpoint = "none";
              options."com.sun:auto-snapshot" = "false";
          };
        };
      };
    };
  };
}

