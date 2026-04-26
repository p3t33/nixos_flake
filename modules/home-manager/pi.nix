{ config, lib, pkgs, pkgs-unstable, ... }:

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
      default = "gpt-5.4";
      description = ''
        Default model ID for the selected provider, for example "gpt-5.4",
        "claude-opus-4-6", or "gemini-2.5-pro".
      '';
    };

    defaultThinkingLevel = lib.mkOption {
      type = lib.types.enum [ "off" "minimal" "low" "medium" "high" "xhigh" ];
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

    skills = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.lines lib.types.path);
      default = { };
      description = ''
        Custom skills as an attrset of `skillName = inlineContent | /path/to/SKILL.md`.
        Skills are stored in .pi/agent/skills/<name>/SKILL.md.
      '';
    };

    extensions = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.lines lib.types.path);
      default = { };
      description = ''
        Custom extensions as an attrset of `extensionName = inlineContent | /path/to/extension.ts`.
        Extensions are stored in .pi/agent/extensions/<name>.ts.
      '';
    };

    secretSkills = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          key = lib.mkOption {
            type = lib.types.str;
            description = ''
              Sops secret key path for the skill content, for example
              "pi-skills/factory-log-analysis".
            '';
          };
        };
      });
      default = { };
      description = ''
        Secret skills as an attrset of `skillName = { key = "sops/secret/path"; }`.
        Decrypted skills are written to .pi/agent/skills/<name>/SKILL.md.
      '';
    };

    promptTemplates = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.lines lib.types.path);
      default = { };
      description = ''
        Prompt templates as an attrset of `templateName = inlineContent | /path/to/template.md`.
        Templates are stored in .pi/agent/prompts/<name>.md and invoked via /<name>.
      '';
    };


  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !(cfg.memory.text != null && cfg.memory.source != null);
        message = "Cannot specify both `custom.programs.pi.memory.text` and `custom.programs.pi.memory.source`";
      }
    ];

    sops.secrets = lib.mkIf (cfg.secretSkills != {}) (lib.mapAttrs' (name: skill:
      lib.nameValuePair "pi-skill-${name}" {
        inherit (skill) key;
        path = "${config.home.homeDirectory}/.pi/agent/skills/${name}/SKILL.md";
      }
    ) cfg.secretSkills);

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

      }
      // lib.mapAttrs' (
        name: content:
        lib.nameValuePair ".pi/agent/skills/${name}/SKILL.md" (
          if lib.isPath content then { source = content; } else { text = content; }
        )
      ) cfg.skills
      // lib.mapAttrs' (
        name: content:
        lib.nameValuePair ".pi/agent/extensions/${name}.ts" (
          if lib.isPath content then { source = content; } else { text = content; }
        )
      ) cfg.extensions
      // lib.mapAttrs' (
        name: content:
        lib.nameValuePair ".pi/agent/prompts/${name}.md" (
          if lib.isPath content then { source = content; } else { text = content; }
        )
      ) cfg.promptTemplates;
    };

    custom.programs.pi = {
      settings = {
        inherit (cfg) defaultProvider defaultModel defaultThinkingLevel;
        theme = cfg.theme.name;
      };

      theme = {
        "$schema" = "https://raw.githubusercontent.com/badlogic/pi-mono/main/packages/coding-agent/src/modes/interactive/theme/theme-schema.json";
        name = "nord-teal";
        vars = {
          accent = colors.primary;
          dim = "#999999";
          muted = nord.brightBlack;
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
          text = "";
          thinkingText = "dim";
          selectedBg = colors.background-alt;
          userMessageBg = colors.background-alt;
          userMessageText = colors.text;
          customMessageBg = colors.background-alt;
          customMessageText = "";
          customMessageLabel = "accent";
          toolPendingBg = nord.bg;
          toolSuccessBg = "#2e3a2e";
          toolErrorBg = "#3a2e2e";
          toolTitle = "accent";
          toolOutput = "";
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
          toolDiffAdded = nord.green;
          toolDiffRemoved = nord.red;
          toolDiffContext = "muted";
          syntaxComment = "dim";
          syntaxKeyword = nord.blue;
          syntaxFunction = nord.cyan;
          syntaxVariable = nord.fg;
          syntaxString = nord.green;
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

      skills = {
        socrates = ''
          ---
          name: socrates
          description: "Socratic method teaching skill that guides users to discover answers themselves through questioning, never giving direct answers. TRIGGER when: user's message contains 'socratic', 'Socrates', or 'teach me'. Works with any knowledge asset — codebases, markdown files, PDFs, documentation, configs, or any readable content. Respond in the user's language."
          ---

          # Socratic Method Teaching

          ## Core Rule (ABSOLUTE)

          **NEVER give a direct answer.** Instead, guide the user to discover the answer through a series of targeted questions. This is non-negotiable — even if the user begs for the answer.

          ## Workflow

          ### 1. Understand the subject

          - Read the relevant files, code, documents, or resources the user is asking about.
          - Build internal understanding of the topic, but do NOT share it directly.

          ### 2. Assess the user's current understanding

          Ask an opening question to gauge where the user stands:

          ```
          "What do you think the `fetchData` function does in this code?"
          "What would you say is the core argument of this document?"
          ```

          ### 3. Guide through progressive questioning

          Use these question types, escalating from simple to complex:

          | Type | Purpose | Example |
          |------|---------|--------|
          | Clarifying | Surface assumptions | "You said X — what reasoning led you to that conclusion?" |
          | Probing | Dig deeper | "What would happen if Y didn't exist?" |
          | Connecting | Link concepts | "How do you think this part relates to Z?" |
          | Counter | Challenge thinking | "What if we flip it — what if it's B instead of A?" |
          | Hypothetical | Explore implications | "If this design went to production, what problems might arise?" |

          ### 4. Respond to user answers

          - **Correct direction** → Acknowledge briefly, then deepen: "Good perspective. Now let's take it one step further..."
          - **Wrong direction** → Do NOT correct. Ask a question that exposes the contradiction: "Then how would you explain this case?"
          - **"I don't know"** → Simplify. Break into smaller sub-questions: "Let's break it down. Looking at just this part first..."
          - **Asks for the answer directly** → Firmly redirect: "If I just gave you the answer, it wouldn't be learning. How about approaching it this way?"

          ### 5. Confirm understanding

          When the user arrives at the answer, ask them to summarize:

          ```
          "Could you summarize what we've discussed so far?"
          ```

          ## Language Rule

          Detect and match the user's language. Always mirror the language the user writes in.

          ## Anti-Patterns (NEVER do these)

          - Stating the answer then asking "do you understand?"
          - Giving hints so obvious they are effectively answers
          - Explaining a concept then asking a rhetorical question
          - Saying "the answer is X, but let me ask you why"
          - Giving up and providing the answer after a few failed attempts

          ## Ending the Session

          When the user demonstrates clear understanding:

          1. Congratulate briefly
          2. Suggest one follow-up question they could explore on their own
          3. Offer to continue the Socratic dialogue on a related topic
        '';

        debugger = ''
          ---
          name: debugger
          description: "Debugging specialist for errors, test failures, and unexpected behavior. Use proactively when encountering any issues."
          ---

          You are an expert debugger specializing in root cause analysis.

          When invoked:
          1. Capture error message and stack trace
          2. Identify reproduction steps
          3. Isolate the failure location
          4. Implement minimal fix
          5. Verify solution works

          Debugging process:
          - Analyze error messages and logs
          - Check recent code changes
          - Form and test hypotheses
          - Add strategic debug logging
          - Inspect variable states

          For each issue, provide:
          - Root cause explanation
          - Evidence supporting the diagnosis
          - Specific code fix
          - Testing approach
          - Prevention recommendations

          Focus on fixing the underlying issue, not the symptoms.
        '';

        log-analyzer = ''
          ---
          name: log-analyzer
          description: "Log analysis specialist for system logs, build failures, and service errors. Use when user mentions 'log', 'journal', 'build failure', 'systemd error', 'boot', 'dmesg', or pastes log output."
          ---

          # Log Analysis

          ## Core Principle

          Logs tell a story. Your job is to reconstruct the narrative — what happened,
          in what order, and why — before proposing any fix.

          ## When to Use

          - System boot failures or service startup issues
          - NixOS build/rebuild errors (nixos-rebuild, nix build)
          - systemd unit failures
          - Kernel panics, hardware errors (dmesg)
          - Application crashes with log output
          - Any pasted log content needing interpretation

          ## Phase 1: Log Intake & Classification

          Before analysis, identify what you're looking at:

          1. **Detect log source** — journald, nix build, dmesg, application log, syslog
          2. **Identify format** — structured (JSON), semi-structured (key=value), free-form
          3. **Parse timestamps** — establish time range, detect timezone
          4. **Assess completeness** — is this a fragment? Are there gaps? Ask for more context if needed.

          If the log is truncated or ambiguous, say so. Suggest commands to get fuller output:
          ```
          journalctl -b -p err..emerg        # current boot errors
          journalctl -b -1                     # previous boot (if boot failure)
          journalctl -u <unit> --since "5m ago"
          nix log <derivation>                 # build log for a specific derivation
          nixos-rebuild switch --show-trace    # full eval trace
          dmesg --level=err,warn               # kernel errors/warnings
          systemctl status <unit> -l --no-pager
          ```

          ## Phase 2: Timeline Reconstruction

          Build a chronological narrative:

          1. **Order events** by timestamp
          2. **Mark phase transitions** — boot stages, service lifecycle (starting → started → failed)
          3. **Identify gaps** — missing time ranges may indicate hangs, crashes, or log rotation
          4. **Tag severity levels** — separate errors/warnings from informational noise
          5. **Note first occurrence** — the first error is usually most significant; later errors often cascade

          Present the timeline as a condensed summary, not a wall of raw log lines.

          ## Phase 3: Pattern Recognition

          Look for:

          | Pattern | Significance |
          |---------|-------------|
          | Repeated identical errors | Retry loop, misconfiguration |
          | Cascading failures | One service taking down dependents |
          | Timeout sequences | Resource contention, deadlock, missing dependency |
          | Permission denied clusters | Wrong user/group, missing capability, SELinux/AppArmor |
          | "not found" errors | Missing package, wrong PATH, broken symlink |
          | OOM killer entries | Memory pressure, unbounded allocation |
          | Dependency ordering | Service started before its dependency was ready |

          ## Phase 4: Cross-Source Correlation

          When multiple log sources are available:

          1. **Align timestamps** across sources
          2. **Trace causality** — kernel event → systemd reaction → service behavior
          3. **Check ordering** — did the dependency start before the dependent?
          4. **Look for environmental triggers** — disk full, network down, OOM

          ## Phase 5: Root Cause Narrowing

          Trace backward from the failure:

          1. Start at the **final symptom** (the error the user sees)
          2. Find the **first error** in the timeline — not the last
          3. Ask: what state was the system in just before that first error?
          4. Identify the **triggering event** or condition
          5. Distinguish root cause from contributing factors

          State your conclusion as: "X happened because Y, triggered by Z."
          If uncertain, present ranked hypotheses with evidence for each.

          ## Phase 6: Investigation Commands

          Always suggest concrete next steps. Tailor to the log source:

          **NixOS / Nix builds:**
          ```
          nix log <drv>                        # full build log
          nix eval --show-trace <expr>         # eval-time errors
          nix-store -q --requisites <drv>      # dependency tree
          nixos-rebuild build --show-trace     # rebuild with trace
          nix why-depends <drv-a> <drv-b>      # why does A need B
          ```

          **systemd / journald:**
          ```
          journalctl -u <unit> -b --no-pager   # full unit log this boot
          journalctl --catalog -b -p err        # annotated errors
          systemctl list-dependencies <unit>    # dependency tree
          systemctl cat <unit>                  # effective unit file
          systemd-analyze blame                 # boot timing
          systemd-analyze critical-chain <unit> # what delayed this unit
          ```

          **Kernel / hardware:**
          ```
          dmesg -T --level=err,warn            # timestamped kernel errors
          journalctl -k -b                     # kernel ring buffer via journal
          lsblk; findmnt                       # storage state
          free -h; cat /proc/meminfo           # memory state
          ```

          ## NixOS-Specific Knowledge

          Common failure modes to recognize:

          - **"infinite recursion encountered"** — circular module option reference; trace with --show-trace
          - **"attribute 'X' missing"** — typo in option name or missing module import
          - **"collision between ... and ..."** — two packages providing same file; use lib.mkForce or priority
          - **"builder for X failed"** — build-time error; get details with `nix log`
          - **"while evaluating"** stack traces — read bottom-up, the deepest frame is the trigger
          - **activation script failures** — check `journalctl -u nixos-activation` or `system.activationScripts`
          - **"hash mismatch in fixed-output derivation"** — upstream source changed; update hash
          - **"cannot coerce null to string"** — option evaluated to null; check option defaults and conditionals

          ## Anti-Patterns

          - Do NOT skip to a fix without reading the full log context
          - Do NOT assume the last error is the root cause
          - Do NOT ignore warnings — they often explain why the error happened
          - Do NOT suggest generic fixes ("try rebuilding") without evidence from the log
          - Do NOT present raw log lines without interpretation
        '';

        grill-me = ''
          ---
          name: grill-me
          description: Interview the user relentlessly about a plan or design until reaching shared understanding, resolving each branch of the decision tree. Use when user wants to stress-test a plan, get grilled on their design, or mentions "grill me".
          ---

          Interview me relentlessly about every aspect of this plan until we reach a shared understanding. Walk down each branch of the design tree, resolving dependencies between decisions one-by-one. For each question, provide your recommended answer.

          Ask the questions one at a time.

          If a question can be answered by exploring the codebase, explore the codebase instead.
        '';
      };

      extensions.fetch-url = ''
        import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
        import { Type } from "@sinclair/typebox";

        function isRedditUrl(url: string): boolean {
          return /^https?:\/\/(www\.|old\.)?reddit\.com\//.test(url);
        }

        function formatRedditComment(comment: any, depth = 0): string {
          if (!comment || comment.kind !== "t1") return "";
          const data = comment.data;
          const indent = "  ".repeat(depth);
          const score = data.score ?? 0;
          const author = data.author ?? "[deleted]";
          let text = indent + author + " (" + score + " points):\n";
          text += data.body
            .split("\n")
            .map((line: string) => indent + "  " + line)
            .join("\n") + "\n\n";
          if (data.replies && data.replies.data && data.replies.data.children) {
            for (const reply of data.replies.data.children) {
              text += formatRedditComment(reply, depth + 1);
            }
          }
          return text;
        }

        async function fetchReddit(url: string, signal?: AbortSignal): Promise<string> {
          const jsonUrl = url.replace(/\/?$/, ".json");
          const resp = await fetch(jsonUrl, {
            signal,
            headers: { "User-Agent": "pi-fetch-url/1.0" },
          });
          if (!resp.ok) throw new Error("Reddit API returned " + resp.status);
          const json = await resp.json() as any[];
          const post = json[0]?.data?.children?.[0]?.data;
          if (!post) throw new Error("Could not parse Reddit response");
          let output = "# " + post.title + "\n\n";
          if (post.selftext) output += post.selftext + "\n\n";
          output += "---\n\n## Comments\n\n";
          const comments = json[1]?.data?.children ?? [];
          for (const comment of comments) {
            output += formatRedditComment(comment);
          }
          return output;
        }

        export default function (pi: ExtensionAPI) {
          pi.registerTool({
            name: "fetch_url",
            label: "Fetch URL",
            description: "Fetch a web page and return its readable text content. Use when the user provides a URL or asks to read/summarize a web page.",
            promptSnippet: "Fetch a URL and extract readable text content",
            parameters: Type.Object({
              url: Type.String({ description: "URL to fetch" }),
            }),
            async execute(_toolCallId, params, signal) {
              const maxLength = 50000;
              let text: string;

              if (isRedditUrl(params.url)) {
                text = await fetchReddit(params.url, signal);
              } else {
                const result = await pi.exec(
                  "${lib.getExe pkgs.nodePackages.readability-cli}",
                  ["--quiet", "-p", "text-content", params.url],
                  { signal, timeout: 30000 }
                );
                if (result.code !== 0) {
                  throw new Error(result.stderr || "Failed to fetch URL");
                }
                text = result.stdout.trim();
              }

              const truncated = text.length > maxLength
                ? text.slice(0, maxLength) + "\n\n[Truncated - content exceeded " + maxLength + " characters]"
                : text;
              return {
                content: [{ type: "text", text: truncated || "No readable content found." }],
                details: {},
              };
            },
          });
        }
      '';

      promptTemplates.commit = ''
        ---
        description: Create a git commit with a clear imperative message
        ---
        Look at `git status` and `git diff --cached`. If nothing is staged, stage the appropriate changes first.

        Create a single atomic commit:
        - Subject: imperative mood, max 50 chars, no period
        - Body: only if the why isn't obvious from the subject
        - No conventional commit types (feat:, fix:, etc.)

        Show the diff summary before committing. Do not add co-authored-by or any AI attribution trailers.
      '';
    };
  };
}
