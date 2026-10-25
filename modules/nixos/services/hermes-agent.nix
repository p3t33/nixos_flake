{ config, hostSpecific, lib, ... }:
{
  options.custom.services.hermes-agent.enable =
    lib.mkEnableOption "Hermes Agent gateway service";

  config = lib.mkIf config.custom.services.hermes-agent.enable {
    services.hermes-agent = {
      enable = true;
      user = hostSpecific.primeUsername;
      group = config.users.users.${hostSpecific.primeUsername}.group;
      createUser = false;

      # OAuth login required per-host: hermes auth add openai-codex
      settings = {
        model = {
          provider = "openai-codex";
          default = config.home-manager.users.${hostSpecific.primeUsername}.custom.programs.pi.defaultModel;
        };
      };

      extraDependencyGroups = [ "messaging" "voice" ];

      environmentFiles = [
        config.sops.secrets."hermes-agent/env".path
      ];

      addToSystemPackages = true;
    };

    sops.secrets."hermes-agent/env" = { };
  };
}
