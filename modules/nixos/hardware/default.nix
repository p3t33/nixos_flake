{
  imports = [
    ./dhcp-setup.nix # no conditional.
    ./host-platform.nix # no conditional.
    ./cpu-microcode.nix # uses variable to set intel/amd.
    ./gpu/nvidia.nix
    ./gpu/nvidia_hybrid_with_intel_offload_mode.nix
    ./gpu/amd.nix
    ./gpu/amd_hardware_decoding.nix
    ./gpu/intel_hardware_decoding.nix
  ];
}
