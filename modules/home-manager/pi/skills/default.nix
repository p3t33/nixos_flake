{ config, lib, pkgs, ... }:

let
  cfg = config.custom.programs.pi;

  youtube-transcript = import ./youtube-transcript.nix { inherit lib pkgs; };
in
{
  options.custom.programs.pi = {
    skills = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.lines lib.types.path);
      default = { };
      description = ''
        Custom skills as an attrset of `skillName = inlineContent | /path/to/SKILL.md`.
        Skills are stored in .pi/agent/skills/<name>/SKILL.md.
      '';
    };

    secretSkills = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            key = lib.mkOption {
              type = lib.types.str;
              description = ''
                Sops secret key path for the skill content, for example
                "pi-skills/factory-log-analysis".
              '';
            };
          };
        }
      );
      default = { };
      description = ''
        Secret skills as an attrset of `skillName = { key = "sops/secret/path"; }`.
        Decrypted skills are written to .pi/agent/skills/<name>/SKILL.md.
      '';
    };

  };

  config = lib.mkIf cfg.enable {
    sops.secrets = lib.mkIf (cfg.secretSkills != { }) (
      lib.mapAttrs' (
        name: skill:
        lib.nameValuePair "pi-skill-${name}" {
          inherit (skill) key;
          path = "${config.home.homeDirectory}/.pi/agent/skills/${name}/SKILL.md";
        }
      ) cfg.secretSkills
    );

    home.file = lib.mapAttrs' (
      name: content:
      lib.nameValuePair ".pi/agent/skills/${name}/SKILL.md" (
        if lib.isPath content then { source = content; } else { text = content; }
      )
    ) cfg.skills // {
      ".pi/agent/skills/youtube-transcript/transcript" = {
        source = youtube-transcript.script;
        executable = true;
      };
    };

    custom.programs.pi = {
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
        socrates = import ./socrates.nix;
        debugger = import ./debugger.nix;
        log-analyzer = import ./log-analyzer.nix;
        learning-planner = import ./learning-planner.nix;
        youtube-transcript = youtube-transcript.skill;
        grill-me = import ./grill-me.nix;
      };
    };
  };
}
