{ config, ... }:
{
  # Nvidia PRIME(technology used to manage hybrid graphics) settings
  # Note: non hybrid Nvidia graphics don't need this part and should be
  # only using the nvidia.nix file.
  hardware.nvidia.prime = {

    # In this mode the Nvidia card is only activated on demand
    # There is also the sync mode in which In this mode the Nvidia card is
    # turned on constantly, having impact on laptop battery and health.
    # And in this mode there might be some issues.
    # In any case it is always a good idia to keep an eye on the official documentation.
    offload = {
      enable = true;
      enableOffloadCmd = true;
    };

    # values in configuration, were found by executing lspci | grep -E 'VGA|3D'
    nvidiaBusId = config.hostSpecification.nvidiaHybridWithIntel.nvidiaBusId;
    intelBusId = config.hostSpecification.nvidiaHybridWithIntel.intelBusId;
  };
}
