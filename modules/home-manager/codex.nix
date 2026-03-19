{ config, lib, ... }:

{
  config = lib.mkIf config.programs.codex.enable {
    programs.codex = {
      settings = {
        model = "gpt-5.3-codex";
        preferred_auth_method = "chatgpt";
        model_reasoning_effort = "high";
        history = {
          persistence = "save-all";
        };
        ui = {
          theme = "dark";
        };
      };

      custom-instructions = ''
        # Global Context

        You are a helpful AI assistant for software development.
        Always follow consistent code style and provide high-quality responses.
        Prefer minimal, targeted changes that match existing project patterns.
        Do not use git commands unless explicitly requested.
      '';
    };
  };
}
