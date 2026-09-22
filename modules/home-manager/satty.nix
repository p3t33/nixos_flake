{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.satty;
  grim = lib.getExe pkgs.grim;
  satty = lib.getExe (if cfg.package == null then pkgs.satty else cfg.package);
  slurp = lib.getExe pkgs.slurp;

  screenshot = pkgs.writeShellScriptBin "satty-screenshot" ''
    set -o pipefail

    geometry="$(${slurp})" || exit 0
    [ -n "$geometry" ] || exit 0

    ${grim} -t ppm -g "$geometry" - | ${satty} --filename -
  '';
in
{
  options.custom.programs.satty.screenshotPackage = lib.mkOption {
    type = lib.types.package;
    readOnly = true;
    internal = true;
    default = screenshot;
    description = "Satty region-capture command used by compositor keybindings.";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.package != null;
        message = "Satty screenshot capture requires programs.satty.package";
      }
    ];

    programs.satty.settings.general = {
      actions-on-enter = [ "save-to-clipboard" ];
      copy-command = lib.getExe' pkgs.wl-clipboard "wl-copy";
      early-exit = true;
      floating-hack = true;
      output-filename = "${config.xdg.userDirs.pictures}/Screenshot-%Y-%m-%d_%H-%M-%S.png";
      resize.mode = "smart";
    };

    home = {
      packages = [ screenshot ];

      activation.createSattyOutputDirectory = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        run ${lib.getExe' pkgs.coreutils "mkdir"} -p ${lib.escapeShellArg config.xdg.userDirs.pictures}
      '';
    };
  };
}
