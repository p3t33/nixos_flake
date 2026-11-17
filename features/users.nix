{ ... }:
{
  flake.modules.nixos.users = { inputs, ... }: {
    imports = [
      inputs.self.modules.nixos.user-primary
      inputs.self.modules.nixos.user-network-backup
    ];
  };
}
