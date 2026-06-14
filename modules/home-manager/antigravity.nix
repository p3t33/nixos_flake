{ config, lib, pkgs-unstable, ... }:

{
  config = lib.mkIf config.programs.antigravity-cli.enable {
    programs.antigravity-cli = {
      package = pkgs-unstable.antigravity-cli;

      # Sets the GEMINI_MODEL environment variable (takes effect after re-login).
      # Antigravity has no settings.json model key; selection is otherwise via /model.
      defaultModel = "gemini-3-flash-preview";

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
          - When a question involves a specific tool, library, or technology with public documentation, fetch and read the relevant docs before answering. Do not rely solely on training data for implementation details, CLI flags, or API behavior.

          ## Coding Standards
          - Git: imperative mood, 50 char subject max, no conventional commit types
          - Prefer self-documenting code over comments
          - Make minimal, targeted changes that match existing project patterns

          ## Safety
          - Never force-push or reset --hard without asking
          - Never touch .env files or secrets
          - Always show diffs before committing
          - Never add co-authored-by or any AI attribution trailers to git commits
        '';
      };

      # Antigravity's settings.json is a sparse, flat schema (see cli-reference docs).
      # Only deliberate non-defaults belong here; everything else is managed via /config.
      settings = {
        editor = "nvim";
      };

      # In native (antigravity) mode the module converts these to skills
      # under the antigravity skills directory, exposed as /<name> slash commands.
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
          description = "Create a git commit.";
          prompt = "Look at the current `git status` and `git diff HEAD`. Create a single atomic git commit with an imperative-mood subject (50 char max, no conventional commit types). Show the diff summary before committing. {{args}}";
        };
      };

      skills = {
        nix-expert = ''
          ---
          name: nix-expert
          description: Expert guidance for Nix/NixOS development, including flake management, module refactoring, and debugging evaluation issues.
          ---
          # Nix Expert Skill

          You are a specialized expert in the Nix ecosystem. When this skill is active, you must adhere to these standards:

          ## Architectural Principles
          - **Modularity**: Prioritize small, focused modules over monolithic files. Use `imports = [ ... ]` to compose functionality.
          - **Separation of Concerns**: Keep `options` (the interface) and `config` (the implementation) distinct.
          - **Reproducibility**: Avoid absolute paths; use relative paths or flake inputs.

          ## Coding Standards
          - **Library vs Builtins**: Always prefer `lib` (e.g., `lib.mapAttrs`) over `builtins` (e.g., `builtins.mapAttrs`) for better error messages and consistency.
          - **Idiomatic Patterns**:
            - Use `mkIf` for conditional blocks.
            - Use `mkMerge` when combining multiple conditional configuration sets.
            - Use `inherit` for concise attribute passing.
          - **Formatting**: Ensure compatibility with `nixfmt-rfc-style`.

          ## Flake Management
          - Use `inputs.nixpkgs.follows` to keep the dependency tree clean.
          - Document flake outputs clearly.
          - Prefer `lib.nixosSystem` for host definitions.
        '';
      };
    };
  };
}
