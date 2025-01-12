{ config, ... }:
{
  sops.secrets.initial_hashed_password.neededForUsers = true;
}
