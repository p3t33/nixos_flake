{
  config,
  lib,
  pkgs,
  pkgs-unstable,
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
    programs.satty.package = pkgs-unstable.satty;

    xdg.configFile."satty/overrides.css".text = ''
      .root {
        background-color: #101216;
        border: 1px solid #d6d9df;
      }

      .outer_box {
        background-color: #101216;
      }

      .toolbar {
        color: #d6d9df;
        background-color: #101216;
      }

      .toolbar button {
        color: #d6d9df;
        background-color: #3b4354;
        background-image: none;
        border-color: transparent;
        box-shadow: none;
      }

      .toolbar button:hover {
        background-color: #495266;
      }

      .toolbar button:checked,
      .toolbar button.editing {
        color: #f1f3f5;
        background-color: #606b80;
      }

      .toolbar button:disabled {
        color: rgba(214, 217, 223, 0.35);
      }

      .toolbar spinbutton {
        color: #d6d9df;
        background-color: rgba(255, 255, 255, 0.04);
        background-image: none;
        border-color: transparent;
        box-shadow: none;
      }

      .toolbar spinbutton text {
        color: #d6d9df;
        background-color: transparent;
      }

      .toolbar spinbutton:focus-within {
        border-color: #606b80;
        box-shadow: inset 0 0 0 1px rgba(96, 107, 128, 0.35);
      }
    '';

    assertions = [
      {
        assertion = cfg.package != null;
        message = "Satty screenshot capture requires programs.satty.package";
      }
    ];

    programs.satty.settings = {
      color-palette.palette = [
        "#ff9f1cff"
        "#eb4d4bff"
        "#6ab04cff"
        "#22a6b3ff"
        "#130f40ff"
      ];
      general = {
        actions-on-enter = [ "save-to-clipboard" ];
        copy-command = lib.getExe' pkgs.wl-clipboard "wl-copy";
        default-fill-shapes = false;
        early-exit = true;
        floating-hack = true;
        initial-tool = "rectangle";
        output-filename = "${config.xdg.userDirs.pictures}/Screenshot-%Y-%m-%d_%H-%M-%S.png";
        resize.mode = "smart";
      };
    };

    home = {
      packages = [ screenshot ];

      activation.createSattyOutputDirectory = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        run ${lib.getExe' pkgs.coreutils "mkdir"} -p ${lib.escapeShellArg config.xdg.userDirs.pictures}
      '';
    };
  };
}
