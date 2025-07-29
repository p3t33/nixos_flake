{ config, hostSpecific, ... }:
{
  system.autoUpgrade = {
    enable = true;
    dates = "Fri *-*-1..7,15..21 01:00:00";
    flake = "github:${config.customGlobal.githubFlakeRepositoryName}#${hostSpecific.hostName}";
    randomizedDelaySec = "5m";
  };
}
