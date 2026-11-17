{ ... }:
{
  flake.modules.homeManager.syncthing = { ... }:
{

  services.syncthing = {
    enable = true;
  };
};
}
