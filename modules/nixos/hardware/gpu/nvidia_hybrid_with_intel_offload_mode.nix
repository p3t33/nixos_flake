#todo
{ config, lib, ... }:
let
  cfg = config.custom.hardware.nvidiaHybridWithIntelOffLoadMode;
in
{
  options = {
    custom = {
      hardware.nvidiaHybridWithIntelOffLoadMode.enable = lib.mkEnableOption "Enable NVIDIA PRIME offload (hybrid with Intel)";
      hardware.nvidiaHybridWithIntel = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = {
          nvidiaBusId = "";
          intelBusId = "";
        };
        description = "Bus IDs for Nvidia Hybrid with Intel setup";
      };
    };
  };
  # Nvidia PRIME(technology used to manage hybrid graphics) settings
  # Note: non hybrid Nvidia graphics don't need this part and should be
  # only using the nvidia.nix file.
  config = lib.mkIf cfg.enable {
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

      # values in configurations(shared.nix) were found by executing lspci | grep -E 'VGA|3D'
      nvidiaBusId = config.custom.hardware.nvidiaHybridWithIntel.nvidiaBusId;
      intelBusId = config.custom.hardware.nvidiaHybridWithIntel.intelBusId;
    };
  };
}
