{ ... }:
{
  flake.modules.nixos.hardware = { inputs, ... }: {
    imports = [
      inputs.self.modules.nixos.dhcp-setup # no conditional.
      inputs.self.modules.nixos.host-platform # no conditional.
      inputs.self.modules.nixos.cpu-microcode # uses variable to set intel/amd.
      inputs.self.modules.nixos.gpu-nvidia
      inputs.self.modules.nixos.gpu-nvidia-hybrid-intel-offload
      inputs.self.modules.nixos.gpu-amd
      inputs.self.modules.nixos.gpu-amd-hardware-decoding
      inputs.self.modules.nixos.gpu-intel-hardware-decoding
    ];
  };
}
