{ config, lib, pkgs, ... }:
let
  cfg = config.services.ollama;
in
{
  config = lib.mkIf cfg.enable {
    services.ollama = {
      # Explicit, predictable choice for THIS machine

      host = config.customGlobal.localHostIPv4;
      port = 11434;
      openFirewall = false;

      loadModels = [
        "llama3:8b" # Meta Llama 3 (open weights)
      ];

      syncModels = true;
    };
  };
}

