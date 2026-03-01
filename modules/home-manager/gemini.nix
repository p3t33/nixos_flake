{ config, lib, ... }:

{
  config = lib.mkIf config.programs.gemini-cli.enable {
    programs.gemini-cli = {
      context = {
        GEMINI = ''
          # Global Context

          You are a helpful AI assistant for software development.
          Always follow consistent code style and provide high-quality responses.
        '';
      };

      commands = {
        explain = {
          description = "Explain the given code or concept.";
          prompt = "Explain this thoroughly and concisely.";
        };
        refactor = {
          description = "Refactor the provided code.";
          prompt = "Refactor this code to be more idiomatic, clean, and maintainable. Follow project conventions.";
        };
        test = {
          description = "Generate unit tests for the selected code.";
          prompt = "Write comprehensive unit tests for this code, covering edge cases and adhering to the existing test framework.";
        };
      };

      settings = {
        model = {
          name = "gemini-3-pro-preview";
        };
        security = {
          auth = {
            selectedType = "oauth-personal";
          };
        };
        general = {
          preferredEditor = config.home.sessionVariables.EDITOR;
          vimMode = true;
          enableAutoUpdate = false;
          enableAutoUpdateNotification = false;
          checkpointing.enabled = true;
          sessionRetention = {
            enabled = true;
            maxAge = "30d";
          };
        };
        ui = {
          showLineNumbers = true;
          showShortcutsHint = true;
          dynamicWindowTitle = true;
          incrementalRendering = true;
          theme = "Default";
        };
        tools = {
          useRipgrep = true;
          shell.enableShellOutputEfficiency = true;
        };
        experimental = {
          plan = true;
          toolOutputMasking.enabled = true;
        };
      };
    };
  };
}
