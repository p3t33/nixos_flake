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

      # === Skill Design Guide ===
      #
      # A skill is a workflow — a sequence of steps with checkpoints that produce
      # evidence, ending in a defined exit criterion. Process over prose.
      #
      # Structure:
      #   - Global preamble: purpose (2-3 lines) + guiding principles that govern
      #     all steps. Principles are the source of truth and go into the plan
      #     artifact, but steps embed their content directly at the point of
      #     decision — no indirection, no "see principle X" references.
      #   - Everything else lives inside steps
      #
      # Step anatomy (consistent order):
      #   - Purpose: what this step achieves and what it works with (the mini prompt)
      #   - Guiding principles (optional): behavioral constraints for steps involving
      #     judgment. Not all steps need them — only those where the agent exercises
      #     discretion (e.g., asking questions, spotting gaps, auditing quality).
      #     Write "do X" not "don't do Y". Include a concrete test for compliance.
      #   - Process: the actual work — what the agent does. May contain sub-headings
      #     for readability (bold, not heading-level).
      #   - Exit criteria: observable evidence the step is done.
      #     The agent shows its work to the user — not self-grades internally.
      #     Steps that feed other steps require a visible summary before proceeding.
      #
      # Pipeline:
      #   - Steps form a forward-only pipeline — no backtracking
      #   - Each step consumes only the previous step's output
      #   - Each step produces a complete artifact, not a delta
      #   - Purpose line anchors the step to its input ("based on X from Step N")
      #
      # Single responsibility:
      #   - Each step owns exactly one concern
      #   - Principles and rules within a step are scoped to that concern
      #   - If a step is doing two things, split it
      #
      # Self-checks vs verification steps:
      #   - Self-checks: internal quality gates within a step, run before presenting
      #     output to the user. The step fixes failures before the user sees anything.
      #   - Verification steps: dedicated steps that audit a previous step's output
      #     from a different angle. The agent shifts from building to auditing mode.
      #   - Use self-checks when the step can validate its own output mechanically.
      #   - Use verification steps when auditing requires a different perspective.
      #
      # Principles placement:
      #   - Embed principle content directly at the point of decision
      #   - Never use indirection ("see Principle X") — the LLM may not look it up
      #   - Global principles exist as source of truth and for the plan artifact
      #   - Step-level rules are self-contained
      #
      # Keep skills lean. Don't over-specify what the model already does well.
      # If the agent cuts corners, add targeted guidance at the failure point.
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

        learning-planner = ''
          ---
          name: learning-planner
          description: "Builds a complete learning plan with curated resources for any domain. Elicits domain-specific details through structured questioning, applies fixed learning principles, and produces a persistent plan artifact. Use via /skill:learning-planner when you want a full learning plan."
          ---

          # Learning Planner

          Build a complete, actionable learning plan with curated resources for any domain — from grilling meat to embedded development to differential math.

          ## Learning Model

          Six principles that govern every decision in this skill. Every structural choice, resource selection, and review check must be justified against these principles.

          1. **Transfer-appropriate practice** — practice mirrors the goal. The learner does the actual activity, not reads about it.
          2. **Pull-based supporting knowledge** — no prerequisite stages. Pull knowledge at point of need when practice surfaces a gap.
          3. **Deliberate effort builds patterns** — learning is building a pattern library through System 2 engagement. Mastery is when patterns become automatic (System 1). Practice must keep the learner in System 2 — if it's automatic, it's not learning.
          4. **Interleaved practice** — vary across topics to strengthen pattern discrimination.
          5. **Spaced repetition** — space practice to consolidate into long-term memory.
          6. **Timely feedback in a high-validity environment** — clear feedback, close to the action, in an environment where real patterns exist.

          ## Step 1: Evaluation

          **Purpose:** Elicit everything needed to build the plan through structured questioning.

          **Guiding principles:**

          - One question at a time. Wait for the answer before proceeding.
          - Every answer must include concrete specifics — what they've done, built, read, or used. If it doesn't, follow up until it does.
          - Confirm each theme is resolved with a specific answer before moving to the next.
          - For each question, provide your recommended answer so the user has something to react to.
          - Never convert a knowledge gap into a prerequisite goal. Gaps are addressed through practice of the primary goal — pull knowledge at point of need, not before it.
          - Extract only. Save all recommendations for Step 4.
          - Thoroughness over speed. A bad plan costs more time than thorough questions.
          - Always ask explicitly. Every theme is resolved by the user's words, not the agent's inference.

          **Process:**

          Cover these themes in order:

          1. **Goal** — a concrete activity the user wants to be able to perform. Cook a steak, not "become a chef." Board bringup from power-on to booting Linux, not "learn embedded." Pass a differential equations test, not "learn differential equations." If the user's goal is broad or identity-based, refine it to the single most impactful activity given their current baseline. Other activities become future tracks, not prerequisites.
          2. **Horizon** — bounded or unbounded. Bounded: test prep, certification, specific project with a finish line — tighter plans, fewer passes, retention targeted to a horizon. Unbounded: career skill, ongoing craft, long-term capability — review checkpoints, reassess-baseline triggers, built for long-term retention. Explain the distinction with examples and get a clear answer — this is a structural decision that shapes the entire plan.
          3. **Success criteria** — what does done look like, minimum bar.
          4. **Baseline** — what do you already know. Probe for specifics: what have you done, built, read, used. Knowledge gaps discovered here are context for resource selection — they are never converted into prerequisite goals or separate stages. Gaps are addressed through practice — pull knowledge at point of need when practice surfaces a gap. If the gap between baseline and goal is so large that the learner cannot meaningfully attempt even a guided practice unit — they lack the minimum vocabulary to engage with the activity — the goal is rescoped closer to the baseline. This is not a prerequisite stage; it is a different, appropriately-scoped goal.
          5. **Assets** — tools, hardware, resources already acquired.
          6. **Constraints** — time, access, deadlines, scope boundaries.
          7. **Concerns** — bottlenecks, worries, known risks.

          **Exit criteria:** All 7 themes have concrete, specific answers. No theme was skimmed or left vague. The user and skill have explicit agreement on a single concrete goal stated as an activity that can be practiced directly. Summarize all 7 themes and their resolved answers back to the user before proceeding. The user explicitly approves or iterates.

          ## Step 2: Blind Spots

          **Purpose:** Take resolved themes from Step 1, zoom out, and surface critical gaps the user didn't think of.

          **Guiding principles:**

          - Focus on what's missing, not what's wrong. The evaluation answers are taken as given.
          - Surface all blind spots found. The user opts out, not in.
          - Show reasoning for every finding — or for finding nothing. Every conclusion requires evidence.

          **Process:**

          - Review the full evaluation as a single body of information.
          - Identify blind spots that would block or seriously hinder progress — things the user couldn't self-assess because they don't know the domain well enough yet.
          - Present findings ranked by criticality with a clear recommendation for each.
          - Ask a focused question or two if needed to resolve ambiguity.
          - Note findings and carry forward. Do not loop back into Step 1.

          **Exit criteria:** Present refined themes — Step 1 output with blind spot findings integrated, gaps filled, risks surfaced with recommendations. This replaces Step 1 output as the single source of truth for all downstream steps. The user explicitly approves or iterates.

          ## Step 3: Structure

          **Purpose:** Propose the track/phase skeleton based on refined themes from Step 2. No resource selection — only the shape of the learning journey.

          **Process:**

          Hierarchy:

          - **Track** — a self-contained learning journey toward the concrete activity goal established in evaluation and refined by blind spots.
          - **Phase** — the atomic practice unit. One complete iteration of doing the thing the learner is trying to learn. A phase has a specific focus area and difficulty level. Each phase must have a clear feedback mechanism — how does the learner know they succeeded or failed?

          **Structural rules:**

          - **Difficulty progression.** Phases increase in difficulty to keep the learner in deliberate effort (System 2) — learning is building a pattern library, if it's automatic it's not learning. If a phase can be completed on autopilot, it's too easy.
          - **Interleave across sub-topics.** Phases vary in focus to strengthen pattern discrimination — don't repeat the same sub-topic in consecutive phases.
          - **Space the repetition.** Phases revisiting similar sub-topics are spaced apart to consolidate into long-term memory.
          - **Pull-based supporting knowledge.** Supporting disciplines are never separate phases or tracks. They are identified by name and trigger condition — pull knowledge at point of need when practice surfaces a gap.
          - **Feedback mechanism.** Every phase must specify how the learner gets timely, clear feedback in a high-validity environment — where real patterns exist to be learned.

          Synthesize refined themes into a skeleton. For each track include:
          - Track name, scope, and success criteria
          - Phases in order, each with: name, goal (one sentence), focus area, difficulty level relative to prior phase, feedback mechanism
          - Sequencing rationale — why this order, how interleaving and difficulty progression are achieved
          - Pull-based supporting knowledge with name and trigger condition
          - Dependencies between tracks (if multi-track)

          **Self-checks** before presenting:

          1. **Principles check** — does every structural decision serve: transfer-appropriate practice, pull-based knowledge, deliberate effort, interleaving, spacing, or timely feedback?
          2. **Coverage check** — every gap and blind spot in the refined themes is addressed by at least one phase. Nothing fell through the cracks.
          3. **Interleaving check** — no two consecutive phases focus on the same sub-topic.
          4. **Difficulty check** — phases progress in difficulty. No phase is easier than the one before it unless it introduces a new sub-topic.
          5. **Feedback check** — every phase has a specified feedback mechanism.

          If any check fails, fix the skeleton before presenting.

          **Exit criteria:** All self-checks pass. Present the skeleton to the user. Block on explicit user approval before proceeding to curation. The user may iterate on the skeleton.

          ## Step 4: Curation

          **Purpose:** Fill the approved skeleton with resources. No structural changes — the skeleton from Step 3 is the contract.

          **Process:**

          Generate the plan by populating the approved skeleton with resources. Do not ask for confirmation before writing — the user has already approved the structure. They can iterate after. If curation surfaces a structural concern, note it in the plan output under a dedicated section — do not stop or backtrack.

          **Resource Taxonomy** — a classification of learning resources by their relationship to practice.

          1. **Structured practice** — walks you through doing the activity. Courses with exercises, project-based books, guided labs. Remove the doing and the resource collapses.
          2. **Drill** — pool of problems to practice on. Teaching material exists to set up the problems, not the other way around.
          3. **Reference** — supports non-sequential access. Pulled when practice surfaces a gap. Includes documentation, cookbooks, best-practices guides.
          4. **Textbook** — sequential instruction. Later material builds on earlier material. Used as driver when no structured practice resource exists.
          5. **Philosophy** — primary value is a mental model or worldview shift. Always pull-based.

          **Curation rules:**

          1. **Maximize return on time invested.** Every resource must justify its time cost. If a shorter or more focused resource teaches the same concept, use it.
          2. **One driver per phase, everything else is pulled.** Each phase has exactly one resource that sets the structure. Every other resource is pull-based — opened when needed, not scheduled alongside.
          3. **Prefer structured practice as driver.** Always prefer a structured practice resource as the driver if one exists — the learner does the actual activity, not reads about it. If none exists, use the best available resource.

          **Plan sections:**

          1. Purpose & Goals
          2. Learning Model — the six principles, included verbatim so the plan can be reviewed independently.
          3. Structural Rules — the track/phase hierarchy and the rules governing it, included verbatim so the plan carries its own evaluation criteria.
          4. Curation Rules — the three curation rules, included verbatim.
          5. Track and phases
          6. Current Baseline (marked provisional for unbounded, with reassessment triggers)
          7. Scope — in scope / out of scope
          8. Blind spots carried forward from Step 2

          **Resource format** — each resource must include:

          - **Name** — title and author/source
          - **Medium** — book, course, video, docs, etc.
          - **Why it fits** — tied to the user's goals and baseline. No "this is popular" justifications.
          - **Where in the sequence** — which track, which phase, order within phase
          - **Type** — one of: structured practice, drill, reference, textbook, philosophy
          - **Role** — driver or pull-based. Exactly one resource per phase is the driver.

          **Exit criteria:** A complete plan covering all sections, with every resource justified and sequenced. The plan matches the approved skeleton — same track, phases, and sequencing.

          ## Step 5: Review

          **Purpose:** The agent audits the complete plan from Step 4. The plan contains its own evaluation criteria — Learning Model, structural rules, curation rules — so review works against the plan itself.

          **Guiding principles:**

          - Every check must show evidence — a passing check needs reasoning, not just "looks good."
          - Fix resource-level issues in place (swap a resource, change a role, adjust a medium). The plan that exits review should be final.
          - Flag structural concerns but do not act on them — the user will address structure through iteration.

          **Process:**

          Run these checks against the plan:

          1. **Skeleton fidelity check** — does the plan match its own approved skeleton? Same track, phases, and sequencing. No additions, removals, or reclassifications.
          2. **Alignment check** — does the plan actually address the goals, baseline, and constraints from the evaluation? Are there gaps between what was discussed and what was planned?
          3. **Principles check** — does each phase's driver have the learner doing the actual activity, not reading about it? Is the learner kept in deliberate effort (System 2)? Is knowledge pulled at point of need, not pre-taught? Are phases interleaved and spaced?
          4. **Curation rules check** — does every resource maximize return on time invested? Is there exactly one driver per phase? Is a structured practice resource used as driver wherever one exists?
          5. **Interleaving check** — do phases vary in focus across sub-topics? If consecutive phases target the same sub-topic, fix it.
          6. **Medium check** — for each resource, would a different medium teach the same concept faster or more effectively? Consider every medium — video, text, hands-on.
          7. **Resource verification** — for every curated resource, verify it exists and is accurately described using `fetch_url`. If it can't be verified, find a verified replacement. If no replacement, flag the gap explicitly.
          8. **Type check** — is each resource's Type label accurate per the Resource Taxonomy? Would a structured practice resource collapse without the doing? Does a drill lose its value without the problems?

          **Exit criteria:** All checks pass. Every resource verified. No unresolved gaps between evaluation and plan.

          ## Step 6: Output

          **Purpose:** Write the plan to a file.

          **Process:**

          - Save the plan to the current working directory.
          - File name format: `{goal}-learning-plan.md`

          **Exit criteria:** File written and path confirmed to the user.
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
