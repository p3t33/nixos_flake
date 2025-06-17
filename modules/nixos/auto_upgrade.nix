{ config, hostSpecific, ... }:
{
  system.autoUpgrade.enable = true;
  system.autoUpgrade.dates = "Fri *-*-1..7,15..21 01:00:00";
  system.autoUpgrade.flake = "github:${config.customGlobalOptions.githubFlakeRepositoryName}#${hostSpecific.hostName}";
  system.autoUpgrade.randomizedDelaySec = "5m";

}
