{ inputs, ... }:
{
  imports = [
    inputs.self.modules.homeManager.sops-common
    inputs.self.modules.homeManager.sops-git-credentials
    inputs.self.modules.homeManager.sops-extra-ssh-hosts
    inputs.self.modules.homeManager.sops-ssh-development-keys-for-vm
    inputs.self.modules.homeManager.sops-calibre
    inputs.self.modules.homeManager.sops-pi-brave-api-key
  ];
}
