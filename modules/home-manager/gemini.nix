{ config, lib, pkgs-unstable, ... }:

{
  config = lib.mkIf config.programs.gemini-cli.enable {
    programs.gemini-cli = {
      package = pkgs-unstable.gemini-cli;
      # defaultModel sets the GEMINI_MODEL environment variable.
      # Session variables only take effect after logout/login.
      #
      # Model selection precedence (highest to lowest):
      #   1. --model CLI flag
      #   2. GEMINI_MODEL env var
      #   3. model.name in settings.json
      #   4. Local Gemma model router (experimental, requires HTTP endpoint in settings.json)
      #   5. Default model ("auto")
      #
      # home-manager has a default value for this variable, and if it is not set at all it will
      # be set to that value and it will implicitly override anything that is set in the settings.model.name.
      # So this configuration must be set. I am setting it to an empty string and not just using it because any
      # change to it will require me to log out and in, which is more pain then just updating settings.model.name.
      defaultModel = "";
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
          - Never add co-authored-by or any AI attribution trailers to git commits
        '';
      };

      settings = {
        model = {
          name = "gemini-3-flash-preview";
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
          previewFeatures = true;
          enableAgents = true;
        };
      };

      # Skills vs Sub-agents:
      # - Skills (SKILL.md) augment the main agent — Gemini autonomously activates them
      #   when the task matches the skill's description, injecting expertise into the
      #   current session. Active for the rest of the conversation once loaded.
      # - Sub-agents are isolated agents delegated a specific bounded task. They run in
      #   a separate context window, do the work, and return a summary to the main agent.
      #   Use them for discrete, offloadable tasks — not for ongoing expertise injection.
      #
      # Commands: named, reusable prompt templates invoked via /command-name [task].
      # Each command has a description and a prompt body that Gemini executes
      # with the provided task as context (passed via {{args}}).
      #
      # Note: In Gemini CLI, these are "Custom Commands" (user-driven shortcuts).
      # They differ from "Skills", which are AI-driven packages of expertise
      # that the model autonomously activates when needed.
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

    };

    # note that unlike claude code there is no way to use /nix-expert to enable the skill instead
    # you need explicitly request it using something like "Enable the nix-expert skill".
    home.file = {
      ".gemini/skills/nix-expert/SKILL.md".text = ''
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

      # agents are called using @<agent_name>.
      ".gemini/agents/code-reviewer.md".text = ''
        ---
        name: code-reviewer
        description: Review code for bugs, logic errors, style issues, and improvements. Use when asked to review a diff, file, or function.
        tools:
          - read_file
          - grep_search
          - glob
        model: gemini-3-pro-preview
        ---

        You are a thorough code reviewer. When given code to review:

        - Identify bugs, logic errors, and edge cases
        - Flag style or naming issues that reduce readability
        - Note missing error handling at system boundaries (user input, external APIs)
        - Suggest simplifications where complexity is unnecessary
        - Be direct and specific — reference line numbers or function names
        - Don't nitpick trivial formatting if a linter handles it

        Return findings as a concise bulleted list grouped by severity: **bugs**, **concerns**, **suggestions**.
        If there is nothing significant to flag in a category, omit it.
      '';

      ".gemini/agents/literal-critic.md".text = ''
        ---
        name: literal-critic
        description: Review blog posts and written content for clarity, prose quality, and structure. Use when asked to review or critique a blog post or article.
        tools:
          - read_file
        model: gemini-3-pro-preview
        ---

        You are a sharp editor reviewing technical blog posts. The blog is kobimedrish.com —
        technical content (NixOS, Linux, embedded systems, developer tooling) written for an
        engineering audience.

        When reviewing:
        - Flag unclear or ambiguous sentences
        - Cut filler words and unnecessary verbosity
        - Check that the introduction sets up what the post actually delivers
        - Verify section flow makes logical sense
        - Point out where examples or explanations could be sharper or more concrete
        - Don't dumb down technical content — the audience knows what they're doing

        Return a structured critique: overall impression first, then specific line-level suggestions.
        Be direct, not diplomatic.
      '';
    };
  };
}
