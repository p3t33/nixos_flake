{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:

let
  cfg = config.custom.programs.pi;
  jsonFormat = pkgs.formats.json { };
  colors = config.custom.shared.colors;
  nord = {
    bg = "#2e3440";
    fg = "#d8dee9";
    black = "#3b4252";
    brightBlack = "#4c566a";
    red = "#bf616a";
    green = "#a3be8c";
    yellow = "#ebcb8b";
    blue = "#81a1c1";
    magenta = "#b48ead";
    cyan = "#88c0d0";
    brightCyan = "#8fbcbb";
  };
in
{
  options.custom.programs.pi = {
    enable = lib.mkEnableOption "pi coding agent (terminal AI harness)";

    defaultProvider = lib.mkOption {
      type = lib.types.str;
      default = "openai-codex";
      description = ''
        Default model provider. Common values include "openai", "openai-codex", "anthropic",
        "google", "github-copilot", and "openrouter".
      '';
    };

    defaultModel = lib.mkOption {
      type = lib.types.str;
      default = "gpt-5.5";
      description = ''
        Default model ID for the selected provider, for example "gpt-5.4",
        "claude-opus-4-6", or "gemini-2.5-pro".
      '';
    };

    defaultThinkingLevel = lib.mkOption {
      type = lib.types.enum [
        "off"
        "minimal"
        "low"
        "medium"
        "high"
        "xhigh"
      ];
      default = "high";
      description = ''
        Default thinking level. Available values: "off", "minimal", "low",
        "medium", "high", "xhigh".
      '';
    };

    settings = lib.mkOption {
      type = jsonFormat.type;
      default = { };
      description = ''
        Pi settings written to ~/.pi/agent/settings.json as a JSON object.
        See https://github.com/badlogic/pi-mono for all supported settings.
      '';
    };

    theme = lib.mkOption {
      type = jsonFormat.type;
      default = { };
      description = ''
        Pi theme definition written to ~/.pi/agent/themes/<name>.json.
        Must include a `name`, `vars`, and all required color tokens.
      '';
    };

    memory = {
      text = lib.mkOption {
        type = lib.types.nullOr lib.types.lines;
        default = null;
        description = ''
          Inline content for ~/.pi/agent/AGENTS.md, or null to disable.
          Mutually exclusive with memory.source.
        '';
      };

      source = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          Path to a file used for ~/.pi/agent/AGENTS.md, or null to disable.
          Mutually exclusive with memory.text.
        '';
      };
    };

  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !(cfg.memory.text != null && cfg.memory.source != null);
        message = "Cannot specify both `custom.programs.pi.memory.text` and `custom.programs.pi.memory.source`";
      }
    ];

    home = {
      packages = [ pkgs-unstable.pi-coding-agent ];

      file = {
        ".pi/agent/settings.json" = lib.mkIf (cfg.settings != { }) {
          source = jsonFormat.generate "pi-settings.json" cfg.settings;
        };

        ".pi/agent/themes/${cfg.theme.name}.json" = lib.mkIf (cfg.theme != { }) {
          source = jsonFormat.generate "pi-theme-${cfg.theme.name}.json" cfg.theme;
        };

        ".pi/agent/AGENTS.md" = lib.mkIf (cfg.memory.text != null || cfg.memory.source != null) (
          if cfg.memory.text != null then { text = cfg.memory.text; } else { source = cfg.memory.source; }
        );
      };
    };

    custom.programs.pi = {
      settings = {
        inherit (cfg) defaultProvider defaultModel defaultThinkingLevel;
        theme = cfg.theme.name;
      };

      theme = {
        "$schema" =
          "https://raw.githubusercontent.com/badlogic/pi-mono/main/packages/coding-agent/src/modes/interactive/theme/theme-schema.json";
        name = "nord-teal";
        vars = {
          accent = colors.primary;
          dim = "#7f8794";
          muted = "#a5abb6";
        };
        colors = {
          accent = "accent";
          border = "accent";
          borderAccent = nord.cyan;
          borderMuted = "muted";
          success = nord.green;
          error = nord.red;
          warning = nord.yellow;
          muted = "muted";
          dim = "dim";
          text = nord.fg;
          thinkingText = "dim";
          selectedBg = colors.background-alt;
          userMessageBg = colors.background-alt;
          userMessageText = nord.fg;
          customMessageBg = colors.background-alt;
          customMessageText = nord.fg;
          customMessageLabel = "accent";
          toolPendingBg = nord.bg;
          toolSuccessBg = "#2e3a2e";
          toolErrorBg = "#3a2e2e";
          toolTitle = "accent";
          toolOutput = "muted";
          mdHeading = nord.yellow;
          mdLink = nord.cyan;
          mdLinkUrl = nord.blue;
          mdCode = nord.cyan;
          mdCodeBlock = "";
          mdCodeBlockBorder = "muted";
          mdQuote = "dim";
          mdQuoteBorder = "muted";
          mdHr = "muted";
          mdListBullet = "accent";
          toolDiffAdded = "#a4e0a0";
          toolDiffRemoved = "#e8a0ac";
          toolDiffContext = "muted";
          syntaxComment = "dim";
          syntaxKeyword = "#88b4f8";
          syntaxFunction = nord.cyan;
          syntaxVariable = nord.fg;
          syntaxString = "#a4e0a0";
          syntaxNumber = nord.magenta;
          syntaxType = nord.brightCyan;
          syntaxOperator = nord.blue;
          syntaxPunctuation = "muted";
          thinkingOff = "muted";
          thinkingMinimal = "dim";
          thinkingLow = nord.blue;
          thinkingMedium = nord.cyan;
          thinkingHigh = "accent";
          thinkingXhigh = nord.magenta;
          bashMode = nord.yellow;
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
        - Always discuss your plan and get explicit approval before executing any actions —
          including reading files, running commands, and exploring the codebase.
        - The only exception is when you need to read documentation to answer a direct question.
        - Discuss approach before writing code. Surface trade-offs and get alignment first.
        - If you discover an issue mid-implementation, stop and discuss.
        - State assumptions explicitly rather than guessing silently.
        - When a question involves a specific tool, library, or technology with public documentation,
          fetch and read the relevant docs before answering. Do not rely solely on training data for
          implementation details, CLI flags, or API behavior.

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
  };
}
