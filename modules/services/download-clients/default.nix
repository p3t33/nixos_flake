{ ... }:
{
  flake.modules.nixos.download-clients = { inputs, ... }: {
    imports = [
      inputs.self.modules.nixos.download-clients-torrent
      inputs.self.modules.nixos.download-clients-usenet
    ];
  };
}
