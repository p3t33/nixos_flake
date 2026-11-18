{ ... }:
{
  flake.modules.nixos.sops-prime-user-hashed-password = { config, ... }:
{
  sops.secrets.initial_hashed_password.neededForUsers = true;
}
;
}
