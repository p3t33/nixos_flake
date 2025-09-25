# uefi only which means that the VM needs to be set with uefi boot
let
  rootPool = "rpool";
  tank = "tank";
in
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
                pool = rootPool;
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
                          pool = tank;
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
                          pool = tank;
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

      tank = {
          type = "zpool";
          mode = {
              topology = {
                  type = "topology";
                  vdev = [
                  {
                      mode = "mirror";
                      members = [ "second" "third" ];
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
