# uefi only which means that the VM needs to be set with uefi boot
let

  pools = { rootPool = "rpool"; tank = "tank"; };

  disks = {
    os = {
      name = "os";
      path = "/dev/disk/by-id/nvme-CT1000P510SSD8_2524E9C358AB";
    };

    ironWolfA = {
      name = "ironWolfA";
      path = "/dev/disk/by-id/ata-ST12000NT001-3LX101_ZRT2EQAF";
      };

    ironWolfB = {
      name = "ironWolfB";
      path = "/dev/disk/by-id/ata-ST12000NT001-3LX101_ZRT2GNH2";
    };
  };

  mirrorMembers = [ disks.ironWolfA.name disks.ironWolfB.name ];

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
                mountOptions = [
                  "defaults"
                ];
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

      "${disks.ironWolfA.name}" = {
          type = "disk";
          device = disks.ironWolfA.path;
          content = {
              type = "gpt";
              partitions = {
                  zfs = {
                      size = "100%";
                      content = {
                          type = "zfs";
                          pool = pools.tank;
                      };
                  };
              };
          };
      };
      "${disks.ironWolfB.name}" = {
          type = "disk";
          device = disks.ironWolfB.path;
          content = {
              type = "gpt";
              partitions = {
                  zfs = {
                      size = "100%";
                      content = {
                          type = "zfs";
                          pool = pools.tank;
                      };
                  };
              };
          };
      };
    };

    zpool = {
      "${pools.rootPool}" = {
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

      "${pools.tank}" = {
          type = "zpool";
          mode = {
              topology = {
                  type = "topology";
                  vdev = [
                  {
                      mode = "mirror";
                      members = mirrorMembers;
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
                  mountpoint = "/data";
              };
              "media" = {
                  type = "zfs_fs";
                  options.mountpoint = "legacy";
                  mountpoint = "/media";
                  options.compression = "lz4";
                  options.recordsize  = "1M";
              };
      };
    };
  };
};
}
