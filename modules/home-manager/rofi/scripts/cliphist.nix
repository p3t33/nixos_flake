{
  config,
  lib,
  pkgs,
}:

let
  cliphist = lib.getExe config.services.cliphist.package;
  mktemp = lib.getExe' pkgs.coreutils "mktemp";
  remove = lib.getExe' pkgs.coreutils "rm";
  timeout = lib.getExe' pkgs.coreutils "timeout";
  rofi = lib.getExe config.programs.rofi.finalPackage;
  wlCopy = lib.getExe' config.services.cliphist.clipboardPackage "wl-copy";
  pangoView = lib.getExe' pkgs.pango "pango-view";
  thumbnailer = lib.getExe' pkgs.gdk-pixbuf "gdk-pixbuf-thumbnailer";
  magick = lib.getExe' pkgs.imagemagick "magick";
  head = lib.getExe' pkgs.coreutils "head";
  iconv = lib.getExe' pkgs.glibc "iconv";
  font = config.custom.shared.font.sansSerif;
  preview = pkgs.writeShellScript "cliphist-preview" ''
    set -euo pipefail
    umask 077
    [[ $# == 3 && "$1" =~ ^(text|image|image-preview):([0-9]+)$ ]] || exit 1
    kind="''${BASH_REMATCH[1]}"
    id="''${BASH_REMATCH[2]}"
    [[ "$3" =~ ^[0-9]+$ ]] || exit 1
    size="$3"
    [[ "$size" == 128 || "$size" == 256 || "$size" == 512 || "$size" == 1024 ]] || exit 1

    if [[ "$size" == 128 && "$kind" != image ]]; then
      exec ${magick} -size 1x1 xc:none "PNG:$2"
    fi
    [[ "$kind" != image-preview ]] || kind=image
    decoded="$(${mktemp} "$CLIPHIST_PREVIEW_DIR/data.XXXXXXXX")"
    trap '${remove} -f -- "$decoded" "$decoded.txt" "$decoded.png"' EXIT
    XDG_CACHE_HOME="$CLIPHIST_PREVIEW_CACHE_HOME" ${timeout} 5 ${cliphist} decode "$id" > "$decoded"
    if [[ "$kind" == image ]]; then
      GDK_PIXBUF_MODULE_FILE=${pkgs.gdk-pixbuf}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache \
        ${timeout} 5 ${thumbnailer} -s "$size" "$decoded" "$2"
      exit
    fi

    ${head} -c 16384 "$decoded" | ${head} -n 400 | ${iconv} -f UTF-8 -t UTF-8 -c > "$decoded.txt" 2>/dev/null || true
    ${timeout} 5 ${pangoView} --no-display --pixels --font=${lib.escapeShellArg "${font} 19"} \
      --width="$((size - 24))" --height="$((size - 54))" --wrap=word-char --ellipsize=end \
      --margin=12 --background=transparent --foreground='#d8dee9' \
      --output="$decoded.png" "$decoded.txt"
    ${timeout} 5 ${magick} "$decoded.png" -gravity NorthWest -crop "''${size}x$((size - 36))+0+0" \
      +repage -background none -extent "''${size}x''${size}" -strip "PNG:$2"
  '';
in
pkgs.writeShellScript "rofi-cliphist" ''
  set -o pipefail
  umask 077
  work="$(${mktemp} -d /tmp/rofi-cliphist.XXXXXXXX)" || exit 1
  trap '${remove} -rf -- "$work"' EXIT
  trap 'exit 1' HUP INT TERM
  export CLIPHIST_PREVIEW_DIR="$work"
  export CLIPHIST_PREVIEW_CACHE_HOME="''${XDG_CACHE_HOME:-$HOME/.cache}"

  ${cliphist} list > "$work/list" || exit 1
  image_pattern='^\[\[ binary data (.+) (png|jpeg|gif|bmp|tiff) ([0-9]+)x([0-9]+) \]\]$'
  image_count=0
  while IFS= read -r entry; do
    id="''${entry%%$'\t'*}"
    [[ "$id" =~ ^[0-9]+$ ]] || continue
    description="''${entry#*$'\t'}"
    label="$entry"
    kind=text
    if [[ "$description" =~ $image_pattern ]]; then
      printf -v label '%s\t%s image · %s×%s · %s' "$id" \
        "''${BASH_REMATCH[2]^^}" "''${BASH_REMATCH[3]}" "''${BASH_REMATCH[4]}" "''${BASH_REMATCH[1]}"
      image_count=$((image_count + 1))
      kind=image
      ((image_count <= 50)) || kind=image-preview
    fi
    printf '%s\0icon\x1fthumbnail://%s:%s\n' "$label" "$kind" "$id"
  done < "$work/list" > "$work/rows"

  selection="$(XDG_CACHE_HOME="$work/cache" ${rofi} -dmenu -display-columns 2 -no-custom -no-markup-rows -show-icons \
    -preview-cmd "${preview} '{input}' '{output}' '{size}'" -theme-str '
    window { width: 1200px; height: 750px; }
    mainbox { children: [ inputbar, message, clipboard-content ]; }
    clipboard-content { orientation: horizontal; children: [ listview, preview-pane ]; }
    listview { lines: 12; fixed-height: true; }
    element { padding: 8px 12px; spacing: 12px; }
    element-icon { size: 28px; }
    preview-pane {
      children: [ icon-current-entry ];
      expand: false;
      padding: 12px;
      border: 1px 0px 0px 1px;
      border-color: @bg1;
    }
    icon-current-entry { size: 512px; expand: true; vertical-align: 0.0; }
  ' < "$work/rows")" || exit 0
  [ -n "$selection" ] || exit 0

  decoded="$work/decoded"
  if ! printf '%s\n' "$selection" | ${cliphist} decode > "$decoded"; then
    exit 1
  fi
  ${wlCopy} < "$decoded"
''
