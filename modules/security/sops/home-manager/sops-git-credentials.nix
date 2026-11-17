{ ... }:
{
  flake.modules.homeManager.sops-git-credentials = {
  sops = {
    secrets.git = { };
  };
}
;
}
