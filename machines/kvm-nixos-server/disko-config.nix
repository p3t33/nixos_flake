# uefi only which means that the VM needs to be set with uefi boot
{
  disko.devices = {
    disk = {
      first = {
        type = "disk";
        device = "/dev/vda";
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
                pool = "boot-pool";
              };
            };
          };
        };
      };
      second = {
          type = "disk";
          device = "/dev/vdb";
          content = {
              type = "gpt";
              partitions = {
                  zfs = {
                      size = "100%";
                      content = {
                          type = "zfs";
                          pool = "tank";
                      };
                  };
              };
          };
      };
      third = {
          type = "disk";
          device = "/dev/vdc";
          content = {
              type = "gpt";
              partitions = {
                  zfs = {
                      size = "100%";
                      content = {
                          type = "zfs";
                          pool = "tank";
                      };
                  };
              };
          };
      };
      fourth = {
          type = "disk";
          device = "/dev/vdd";
          content = {
              type = "gpt";
              partitions = {
                  zfs = {
                      size = "100%";
                      content = {
                          type = "zfs";
                          pool = "tank";
                      };
                  };
              };
          };
      };
    };
    zpool = {
      boot-pool = {
        type = "zpool";
        options = {
          ashift = "12";
        };
        rootFsOptions = {
          mountpoint = "none";
          acltype = "posixacl";
          xattr = "sa";
          atime = "off";
          # encryption = "on";
          # keyformat = "passphrase";
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

      tank = {
          type = "zpool";
          mode = {
              topology = {
                  type = "topology";
                  vdev = [
                  {
                      mode = "raidz1";
                      members = [ "second" "third" "fourth" ];
                  }
                  ];
              };
          };
          options = {
              ashift = "12";
          };
          rootFsOptions = {
              mountpoint = "none";
              compression = "zstd";
              atime = "off";
          };
          datasets = {
              "data" = {
                  type = "zfs_fs";
                  options.mountpoint = "legacy";
                  mountpoint = "/data"; # you can change this to /srv, /mnt/media, etc.
              };
          };
      };
    };
  };
}
