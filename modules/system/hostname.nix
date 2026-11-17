{ ... }:
{
  flake.modules.nixos.hostname = {
  hostSpecific,
  ...
}:
{
  networking.hostName = hostSpecific.hostName;
};
}
