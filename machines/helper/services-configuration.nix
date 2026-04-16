{ ... }:
{
  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };
  };
}
