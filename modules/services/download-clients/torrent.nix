{ ... }:
{
  flake.modules.nixos.download-clients-torrent = { inputs, ... }: {
    imports = [
      inputs.self.modules.nixos.deluge
      inputs.self.modules.nixos.qbittorrent-nox
    ];
  };
}
