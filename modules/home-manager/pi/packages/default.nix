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
# Package sources:
#   Local package paths can point at sources fetched into the nix store, which
#   keeps those packages under nix control. NPM package specs such as
#   `npm:pi-mcp-adapter` are resolved by pi at runtime into ~/.pi/agent/npm.
#
# Fetching strategy — fetchFromGitHub vs buildNpmPackage:
#   Use a local nix-store path for packages we intentionally vendor through
#   nix. Use an npm spec for upstream packages we want pi to install and manage
#   through ~/.pi/agent/npm.
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
      List of pi package specs or local paths. These are added to the `packages`
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
