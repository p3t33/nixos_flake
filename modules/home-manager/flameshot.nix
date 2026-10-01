{
  config,
  lib,
  pkgs,
  pkgs-unstable,
  ...
}:

let
  cfg = config.services.flameshot;
  qt = pkgs-unstable.qt6;
  screenNamesSource = pkgs-unstable.writeText "flameshot-screen-names.cpp" ''
    #include <QGuiApplication>
    #include <QJsonArray>
    #include <QJsonDocument>
    #include <QScreen>
    #include <cstdio>

    int main(int argc, char *argv[]) {
        QGuiApplication::setDesktopSettingsAware(false);
        QGuiApplication app(argc, argv);
        QJsonArray names;
        for (const QScreen *screen : QGuiApplication::screens()) {
            names.append(screen->name());
        }
        const QByteArray json = QJsonDocument(names).toJson(QJsonDocument::Compact);
        return std::printf("FLAMESHOT_SCREENS=%s\n", json.constData()) < 0 ? 1 : 0;
    }
  '';
  screenNames = pkgs-unstable.stdenv.mkDerivation {
    name = "flameshot-screen-names";
    dontUnpack = true;
    dontWrapQtApps = true;
    strictDeps = true;
    nativeBuildInputs = [ pkgs-unstable.pkg-config ];
    buildInputs = [ qt.qtbase ];
    buildPhase = ''
      runHook preBuild
      $CXX -std=c++17 -O2 -fPIC ${screenNamesSource} $(pkg-config --cflags --libs Qt6Gui) -o flameshot-screen-names
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -Dm755 flameshot-screen-names "$out/bin/flameshot-screen-names"
      runHook postInstall
    '';
    meta.mainProgram = "flameshot-screen-names";
  };
  focusedScreenshot = pkgs.writeShellApplication {
    name = "flameshot-focused";
    text = ''
      if [[ $# -gt 1 || ( $# -eq 1 && "$1" != --dry-run ) ]]; then
        printf 'Usage: flameshot-focused [--dry-run]\n' >&2
        exit 2
      fi

      output="$(${lib.getExe' pkgs.sway "swaymsg"} -t get_outputs -r |
        ${lib.getExe pkgs.jq} -er '
          [.[] | select(.active and .focused) | .name] |
          if length == 1 then .[0] else error("No unique focused Sway output") end
        ')"

      if ! screen_names="$(${lib.getExe' pkgs.coreutils "env"} \
        -u QML_IMPORT_PATH -u QML2_IMPORT_PATH \
        -u QT_QPA_PLATFORMTHEME -u QT_STYLE_OVERRIDE \
        QT_QPA_PLATFORM=wayland \
        QT_PLUGIN_PATH=${qt.qtbase}/lib/qt-6/plugins:${qt.qtwayland}/lib/qt-6/plugins \
        QT_FORCE_STDERR_LOGGING=1 QT_MESSAGE_PATTERN='%{message}' \
        ${lib.getExe' pkgs.coreutils "timeout"} 5 \
        ${lib.getExe screenNames} 2>&1)"; then
        printf 'Failed to query Qt screens:\n%s\n' "$screen_names" >&2
        exit 1
      fi

      screen_index="$(printf '%s\n' "$screen_names" |
        ${lib.getExe pkgs.jq} -Rser --arg output "$output" '
          split("\n") |
          map(select(startswith("FLAMESHOT_SCREENS=")) |
            ltrimstr("FLAMESHOT_SCREENS=") | fromjson) |
          if length == 1 then
            .[0] | index($output) // error("Focused output not found in Qt screens")
          else
            error("Could not read Qt screen list")
          end
        ')"

      printf 'Focused output: %s; Flameshot screen: %s\n' "$output" "$screen_index" >&2
      if [[ "''${1:-}" == --dry-run ]]; then
        exit 0
      fi

      exec ${lib.getExe cfg.package} screen --number "$screen_index" --edit
    '';
  };
in
{
  options.custom.programs.flameshot.focusedScreenshotPackage = lib.mkOption {
    type = lib.types.package;
    readOnly = true;
    internal = true;
    default = focusedScreenshot;
    description = "Flameshot region editor on the focused Sway output.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ focusedScreenshot ];

    services.flameshot = {
      package = pkgs-unstable.flameshot;
      settings = {
        General = {
          contrastOpacity = 100;
          contrastUiColor = "#606b80";
          disabledTrayIcon = true;
          drawColor = "#ff9f1c";
          drawThickness = 2;
          savePath = config.xdg.userDirs.pictures;
          showHelp = false;
          showStartupLaunchMessage = false;
          uiColor = "#a800e0";
        };
        Shortcuts = {
          TYPE_ACCEPT = "";
          TYPE_COPY = "Return";
        };
      };
    };
  };
}
