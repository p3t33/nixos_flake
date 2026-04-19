{ pkgs-unstable, ... }:
{
  services.ollama = {
    enable = true;
    package = pkgs-unstable.ollama-cpu;
  };

  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };
  };
}
