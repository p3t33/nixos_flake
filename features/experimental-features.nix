{ ... }:
{
  flake.modules.nixos.experimental-features = { pkgs, ... }:
{
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
};
}
