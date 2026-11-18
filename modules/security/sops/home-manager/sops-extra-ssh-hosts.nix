{ ... }:
{
  flake.modules.homeManager.sops-extra-ssh-hosts = {
  sops.secrets.extra_hosts = { };
}
;
}
