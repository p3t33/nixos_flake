{ config, lib, ... }:

{
  config = lib.mkIf config.programs.claude-code.enable {
    programs.claude-code = {
      settings = {
        model = "claude-sonnet-4-6";
        theme = "dark";
        includeCoAuthoredBy = false;
        permissions = {
          defaultMode = "acceptEdits";
        };
      };

      memory.text = ''
        # Global Context

        You are a helpful AI assistant for software development.
        Always follow consistent code style and provide high-quality responses.
      '';

      commands = {
        explain = ''
          ---
          description: Explain the given code or concept.
          ---
          Explain this thoroughly and concisely.
        '';
        refactor = ''
          ---
          description: Refactor the provided code.
          ---
          Refactor this code to be more idiomatic, clean, and maintainable. Follow project conventions.
        '';
        test = ''
          ---
          description: Generate unit tests for the selected code.
          ---
          Write comprehensive unit tests for this code, covering edge cases and adhering to the existing test framework.
        '';
      };
    };
  };
}
