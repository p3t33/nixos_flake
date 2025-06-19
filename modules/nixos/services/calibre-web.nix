{ pkgs, lib, config, ... }:

let
  calibreLibraryPath = "${config.customHostSpecificGlobalOptions.pathToMediaDirectory}/calibre";
  dummyBookPath = "/tmp/calibre_dummy_book.txt";
in
{
  config = lib.mkIf config.services.calibre-web.enable {
    environment.systemPackages = with pkgs; [
      calibre
      sqlite
    ];

    # create directory for calibre database if it does not exist yet.
    systemd.tmpfiles.rules = [
      "d ${calibreLibraryPath} 0770 ${config.services.calibre-web.user} ${config.customGlobal.mediaGroup} -"
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
        package = pkgs.calibre-web;
        user = "calibre-web";
        group = config.customGlobal.mediaGroup;
        listen = {
          ip = "${config.customGlobal.anyIPv4}";
          port = 8083;
        };
        openFirewall = true;
        options = {
          calibreLibrary = calibreLibraryPath;
          enableBookUploading = true;
          enableBookConversion = true;
        };
    };
  };
}
