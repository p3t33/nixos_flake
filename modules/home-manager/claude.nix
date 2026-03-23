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

      commands = {
        explain = ''
          ---
          description: Explain the given code or concept.
          ---
          Explain this thoroughly and concisely. If it's Nix code, explain the module system patterns used.
        '';
        refactor = ''
          ---
          description: Refactor the provided code.
          ---
          Refactor this code to be more idiomatic, clean, and maintainable. For Nix, prefer lib functions over builtins, use mkIf/mkMerge patterns, and ensure nixfmt-rfc-style compliance.
        '';
        test = ''
          ---
          description: Generate tests for the selected code.
          ---
          Write comprehensive tests for this code, covering edge cases and adhering to the existing test framework.
        '';
        commit = ''
          ---
          description: Create a git commit with a conventional commit message.
          ---
          Look at the current `git status` and `git diff HEAD`. Create a single atomic git commit using conventional commit format (feat:, fix:, refactor:, docs:, chore:). Show the diff summary before committing.
        '';
      };
    };
  };
}
