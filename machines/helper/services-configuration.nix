{ pkgs-unstable, ... }:
{
  services.ollama = {
    enable = true;
    package = pkgs-unstable.ollama-cpu;
    loadModels = [
      # OpenClaw memory search uses Ollama embeddings on helper.
      # Keep the embedding model authoritative here because syncModels prunes
      # anything not declared by the host.
      "nomic-embed-text"
    ];
  };

  custom = {
    profiles.systemServices = {
      core.enable = true;
      server.enable = true;
    };
  };
}
