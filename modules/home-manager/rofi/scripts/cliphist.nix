{
  config,
  lib,
  pkgs,
}:

let
  cliphist = lib.getExe config.services.cliphist.package;
  mktemp = lib.getExe' pkgs.coreutils "mktemp";
  remove = lib.getExe' pkgs.coreutils "rm";
  rofi = lib.getExe config.programs.rofi.finalPackage;
  wlCopy = lib.getExe' config.services.cliphist.clipboardPackage "wl-copy";
in
pkgs.writeShellScript "rofi-cliphist" ''
  set -o pipefail

  selection="$(${cliphist} list | ${rofi} -dmenu -display-columns 2 -no-custom)" || exit 0
  [ -n "$selection" ] || exit 0

  decoded="$(${mktemp})"
  trap '${remove} -f -- "$decoded"' EXIT

  if ! printf '%s\n' "$selection" | ${cliphist} decode > "$decoded"; then
    exit 1
  fi

  ${wlCopy} < "$decoded"
''
