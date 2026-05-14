# Pi packages — external bundles discovered via `package.json`'s `pi` key.
#
# Why packages exist (separate from extensions):
#   Extensions are individual .ts files in ~/.pi/agent/extensions/ that each
#   register a single tool — we write these inline in nix. Packages are
#   third-party projects with a package.json whose `pi` key can bundle
#   multiple extensions, skills, prompts, and themes. We cannot inline them;
#   they must be fetched as-is so pi's package discovery reads their
#   package.json and loads their entry points.
#
# Why packages live in the nix store:
#   `pi install` would write to ~/.pi and mutate state outside of nix's
#   control, breaking reproducibility. Instead we fetch packages into the
#   nix store and point pi at them via settings.json `packages` paths.
#
# Fetching strategy — fetchFromGitHub (default) vs buildNpmPackage:
#   Pi loads .ts packages at runtime via jiti — no build step needed.
#   Most pi packages (like pi-subagent) ship raw TypeScript with only
#   peerDependencies resolved by pi's own node_modules, so fetchFromGitHub
#   is the right default: it's a plain source fetch, no node/npm involved.
#   Use buildNpmPackage only when a package requires a compile or bundle
#   step, or has real runtime dependencies that must be installed into its
#   own node_modules.
{
  config,
  lib,
  ...
}:

let
  cfg = config.custom.programs.pi;
in
{
  imports = [
    ./pi-subagent.nix
  ];

  options.custom.programs.pi.packages = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = ''
      List of local paths to pi packages. These are added to the `packages`
      array in settings.json. Pi discovers extensions, skills, prompts, and
      themes via each package's package.json `pi` key.
    '';
  };

  config = lib.mkIf cfg.enable {
    custom.programs.pi.settings = lib.mkIf (cfg.packages != [ ]) {
      packages = cfg.packages;
    };
  };
}
