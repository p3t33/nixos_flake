{ config, lib, ... }:

{
  config = lib.mkIf config.programs.gemini-cli.enable {
    programs.gemini-cli = {
      context = {
        GEMINI = ''
          # Global Context

          ## Communication
          - Talk to me as a peer engineer, not an assistant. Be direct, push back on bad ideas.
          - No sycophancy — skip "Great question!", "Excellent point!", "You're absolutely right".
          - When something is a subjective preference, say so. Don't frame opinions as facts.
          - Assume I understand common programming concepts without over-explaining.
          - Keep responses concise. Every token should be purposeful.

          ## Workflow
          - Discuss approach before writing code. Surface trade-offs and get alignment first.
          - If you discover an issue mid-implementation, stop and discuss.
          - State assumptions explicitly rather than guessing silently.

          ## Coding Standards
          - Git: conventional commits — <type>(<scope>): <subject>, imperative mood, 50 char subject max
          - Prefer self-documenting code over comments
          - Make minimal, targeted changes that match existing project patterns

          ## Safety
          - Never force-push or reset --hard without asking
          - Never touch .env files or secrets
          - Always show diffs before committing
        '';
      };

      commands = {
        explain = {
          description = "Explain the given code or concept.";
          prompt = "Explain this thoroughly and concisely. If it's Nix code, explain the module system patterns used. {{args}}";
        };
        refactor = {
          description = "Refactor the provided code.";
          prompt = "Refactor this code to be more idiomatic, clean, and maintainable. For Nix, prefer lib functions over builtins, use mkIf/mkMerge patterns, and ensure nixfmt-rfc-style compliance. {{args}}";
        };
        test = {
          description = "Generate tests for the selected code.";
          prompt = "Write comprehensive tests for this code, covering edge cases and adhering to the existing test framework. {{args}}";
        };
        commit = {
          description = "Create a git commit with a conventional commit message.";
          prompt = "Look at the current `git status` and `git diff HEAD`. Create a single atomic git commit using conventional commit format (feat:, fix:, refactor:, docs:, chore:). Show the diff summary before committing. {{args}}";
        };
      };

      defaultModel = "gemini-1.5-pro-latest";
      settings = {
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
          previewFeatures = true;
        };
      };
    };
  };
}
