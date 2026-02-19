{ ... }:
{
  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
      xmr-miner.enable = true;
    };
  };
}
