{ config, lib, pkgs, ... }:

let
  cfg = config.custom.programs.pi;

  piSubagent = pkgs.fetchFromGitHub {
    owner = "mjakl";
    repo = "pi-subagent";
    name = "pi-subagent";
    rev = "f386f10838eb9175253d212a201a6dae2cfe5366";
    hash = "sha256-25Pdq0ic7m3WzAAZ0x/R074erJmqvZF435iZPGtzIiM=";
  };
in
{
  config = lib.mkIf cfg.enable {
    custom.programs.pi.packages = [
      "${piSubagent}"
    ];

    # Subagent definitions — three roles that Claude Code, Codex, and Gemini CLI
    # all converge on as built-in agents. Descriptions are instructions to the
    # parent agent (injected into its prompt) telling it when and how to delegate.
    # Bodies are kept minimal — pi's default system prompt is already sufficient
    # for the child; the description does the heavy lifting.

    home.file = {
      ".pi/agent/agents/explorer.md".text = ''
        ---
        name: explorer
        description: >-
          Use explorer for specific codebase questions. Explorers are fast and
          read-only. They must be used to ask specific, well-scoped questions
          about the codebase. Spawn multiple explorers in parallel when you have
          multiple distinct questions that can be answered independently. Trust
          explorer results without additional verification.
        thinking: low
        tools: read,bash,grep,find,ls
        ---
      '';

      ".pi/agent/agents/code-reviewer.md".text = ''
        ---
        name: code-reviewer
        description: >-
          Use code-reviewer after code changes to catch issues before they land.
          Reviewers are read-only and focus on correctness, edge cases, test
          coverage, and unintended side effects. They cite file paths and line
          numbers as evidence. Do not invent issues — only report problems
          justified by evidence. If everything looks good, say so plainly.
        thinking: high
        tools: read,bash,grep,find,ls
        ---
      '';

      ".pi/agent/agents/worker.md".text = ''
        ---
        name: worker
        description: >-
          Use worker for execution and production work: implementing features,
          fixing bugs, refactoring code, and multi-file edits. When delegating,
          explicitly assign ownership of files and responsibilities. Tell
          workers they are not alone in the codebase — they should not revert
          edits made by others and should adjust their implementation to
          accommodate parallel changes.
        thinking: high
        tools: read,bash,edit,write,grep,find,ls
        ---
      '';
    };
  };
}
