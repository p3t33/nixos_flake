{ config, lib, pkgs, ... }:
let
  cfg = config.custom.programs.aichat;
in
{
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

