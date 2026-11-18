{ ... }:
{
  flake.modules.nixos.media = { inputs, ... }: {
    imports = [
      inputs.self.modules.nixos.audiobookshelf
      inputs.self.modules.nixos.bazarr
      inputs.self.modules.nixos.jackett
      inputs.self.modules.nixos.jellyfin
      inputs.self.modules.nixos.kavita
      inputs.self.modules.nixos.prowlarr
      inputs.self.modules.nixos.radarr
      inputs.self.modules.nixos.recyclarr
      inputs.self.modules.nixos.sonarr
    ];
  };
}
