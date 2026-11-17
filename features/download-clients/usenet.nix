{ ... }:
{
  flake.modules.nixos.download-clients-usenet = { inputs, ... }: {
    imports = [
      inputs.self.modules.nixos.sabnzbd
    ];
  };
}
