{ pkgs, config, ... }:

let
  calibreLibraryPath = "${config.userDefinedGlobalVariables.pathToMediaDirectory}/calibre";
  dummyBookPath = "/tmp/calibre_dummy_book.txt";
in
{
  environment.systemPackages = with pkgs; [
    calibre
    sqlite
  ];

  # create directory for calibre database if it does not exist yet.
  systemd.tmpfiles.rules = [
    "d ${calibreLibraryPath} 0770 ${config.services.calibre-web.user} ${config.userDefinedGlobalVariables.mediaGroup} -"
  ];

  # Create a database for calibre if it does not exist yet.
  # This requires a book, so I am using a dummy one.
  system.activationScripts.initCalibreLibrary.text = ''
    if [ ! -f /mnt/media/calibre/metadata.db ]; then
      touch ${dummyBookPath}
      runuser -u calibre-web -- ${pkgs.calibre}/bin/calibredb add ${dummyBookPath} --with-library ${calibreLibraryPath}
    fi
  '';

  services.calibre-web = {
      enable = true;
      package = pkgs.calibre-web;
      user = "calibre-web";
      group = config.userDefinedGlobalVariables.mediaGroup;
      listen = {
        ip = "${config.userDefinedGlobalVariables.anyIPv4}";
        port = 8083;
      };
      openFirewall = true;
      options = {
        calibreLibrary = calibreLibraryPath;
        enableBookUploading = true;
        enableBookConversion = true;
      };
  };
}
