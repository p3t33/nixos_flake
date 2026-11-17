{ inputs, ... }:
{
  imports = [
    inputs.self.modules.nixos.sops-common
    inputs.self.modules.nixos.sops-prime-user-hashed-password
  ];
}
