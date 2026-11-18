{ inputs, ... }:
{
  imports = [
    inputs.self.modules.homeManager.sops-common
    inputs.self.modules.homeManager.sops-git-credentials
    inputs.self.modules.homeManager.sops-extra-ssh-hosts
    inputs.self.modules.homeManager.sops-ssh-development-keys-for-vm
    inputs.self.modules.homeManager.sops-mcp-gateway
    inputs.self.modules.homeManager.sops-pi-brave-api-key
  ];

  custom.programs.pi.secretSkills.factory-log-analysis.key = "pi-skills/factory-log-analysis";
  custom.programs.pi.secretSkills.log-svx-app.key = "pi-skills/log-svx-app";
  custom.programs.navi.secretCheats.private.key = "navi/private-cheats";
}
