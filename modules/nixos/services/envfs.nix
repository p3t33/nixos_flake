{ ... }:
{
  # Used to handle shebang in scripts in nix
  # export ENVFS_RESOLVE_ALWAYS=1 ...
  services.envfs.enable = true;
}
