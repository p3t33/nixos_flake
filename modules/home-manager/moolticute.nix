{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.custom.programs.moolticute;

  # TODO: Revisit this light-theme workaround after upgrading Moolticute.
  # Upstream 1.03 hard-codes colors that make fields unreadable with dark themes.
  moolticuteWrapped = pkgs.symlinkJoin {
    inherit (pkgs.moolticute) meta;
    name = "${lib.getName pkgs.moolticute}-gui-wrapped-${lib.getVersion pkgs.moolticute}";
    paths = [ pkgs.moolticute ];
    preferLocalBuild = true;
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/moolticute \
        --set GTK_THEME Adwaita \
        --set QT_QPA_PLATFORMTHEME gtk3 \
        --set QT_STYLE_OVERRIDE Fusion
    '';
  };
in
{
  options.custom.programs.moolticute.enable = lib.mkEnableOption "Moolticute GUI";

  config = lib.mkIf cfg.enable {
    home.packages = [ moolticuteWrapped ];

    xdg.desktopEntries.moolticute = {
      name = "Moolticute";
      comment = "Start Moolticute daemon and App";
      exec = "${moolticuteWrapped}/bin/moolticute";
      icon = "moolticute";
      terminal = false;
      categories = [ "Utility" ];
    };
  };
}
