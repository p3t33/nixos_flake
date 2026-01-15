{ config, lib, pkgs, ... }:
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
      ai = "aichat --model ollama:llama3:8b";
    };

    xdg.configFile."aichat/config.yaml".text = ''
      model: ollama:llama3:8b
      clients:
      - type: openai-compatible
        name: ollama
        api_base: http://127.0.0.1:11434/v1
        api_key: ollama
        models:
        - name: llama3:8b
        '';
  };
}

