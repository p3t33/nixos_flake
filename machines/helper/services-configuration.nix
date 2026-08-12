{ ... }:
{
  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };

    services.hermes-agent.enable = true;
  };
}
