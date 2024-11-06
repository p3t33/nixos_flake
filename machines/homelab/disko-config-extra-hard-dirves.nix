# disko configuration files serve a double purpose. They are used to format
# the hard drives and to mount them as part of NixOS configurations.
# This file represents the hard drives that are not to be formatted but only
# mounted.
#
# disko will format everything it is presented with and so the disks that
# are only meant to be mounted by NixOS must be separated in their own file.
{ config, ... }:
{
  disko.devices = {
    disk = {
      sdb = {
        device = "/dev/sdb";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            media = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "${config.userDefinedGlobalVariables.pathToMediaDirectory}";
              };
            };
          };
        };
      };

      sdc = {
        device = "/dev/sdc";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            data = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "${config.userDefinedGlobalVariables.pathToDataDirectory}";
              };
            };
          };
        };
      };
    };
  };
}
