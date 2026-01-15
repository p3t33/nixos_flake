{ config, lib, pkgs, osConfig, ... }:
let
  cfg = config.custom.programs.aichat;
in
{
  # Can be used both with local models(via ollama) and with remote once.
  # intended for level 1(prompt) and 2(file scoped) models(as described in ollama.nix file).
  options.custom.programs.aichat.enable = lib.mkEnableOption "Enable aichat (local Ollama backend)";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.aichat ];

    programs.zsh.shellAliases = {
      ai = "aichat --model ollama:${config.customGlobal.AIDefaultModels.prompt}";
    };

    xdg.configFile."aichat/config.yaml".text = ''
      model: ollama:${config.customGlobal.AIDefaultModels.prompt}
      clients:
      - type: openai-compatible
        name: ollama
        api_base: http://${osConfig.services.ollama.host}:${builtins.toString osConfig.services.ollama.port}/v1
        api_key: ollama
        models:
        - name: ${config.customGlobal.AIDefaultModels.prompt}
        '';
  };
}

