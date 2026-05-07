{ config, lib, ... }:

let
  cfg = config.custom.programs.pi;
in
{
  options.custom.programs.pi = {
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
    home.file = lib.mapAttrs' (
      name: content:
      lib.nameValuePair ".pi/agent/prompts/${name}.md" (
        if lib.isPath content then { source = content; } else { text = content; }
      )
    ) cfg.promptTemplates;

    custom.programs.pi = {
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
