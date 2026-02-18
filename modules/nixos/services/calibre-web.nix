{ pkgs, lib, config, ... }:

let
  calibreLibraryPath = "${config.customGlobal.pathToMediaDirectory}/calibre";
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

    # Calibre-web is just a web based gui front end that requires an actual database to work
    # it will not create one if it does not exist.
    #
    # This one shot service is responsible for creating that database, and works around some
    # limitations of the calibredb cli tool. For database to be created form the cli a single
    # book needs to exist at the time of the creation of the database.
    #
    # So this one shot service:
    # 1. checks if there is an existing database.
    # 2. If there is no database, creates dummy book and uses it to initialize the database.
    # 3. Waits for a few seconds and then removes the database database so the user will
    # see a clean slate first time he opens Calibre-web.
    systemd.services.initCalibreLibrary = {
      description = "Initialize Calibre library with a dummy book if metadata.db is missing";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "init-calibre-library" ''
          set -euo pipefail

          echo "Checking for Calibre metadata.db in ${calibreLibraryPath}"
          if [ ! -f "${calibreLibraryPath}/metadata.db" ]; then
            echo "No metadata.db found, initializing Calibre library..."

            touch ${dummyBookPath}
            echo "Adding dummy book to trigger metadata.db creation"
            ${lib.getExe' pkgs.util-linux "runuser"} -u ${config.services.calibre-web.user} -- \
              ${lib.getExe' pkgs.calibre "calibredb"} add ${dummyBookPath} \
              --with-library ${calibreLibraryPath}
            sleep 2

            echo "Find id of dummy book to be removed"
            book_id=$(${lib.getExe' pkgs.util-linux "runuser"} -u ${config.services.calibre-web.user} -- \
              ${lib.getExe' pkgs.calibre "calibredb"} list --with-library ${calibreLibraryPath} \
              | ${pkgs.gawk}/bin/awk '/calibre_dummy_book/{print $1}')

            if [ -n "$book_id" ]; then
              ${lib.getExe' pkgs.util-linux "runuser"} -u ${config.services.calibre-web.user} -- \
                ${lib.getExe' pkgs.calibre "calibredb"} remove "$book_id" --with-library ${calibreLibraryPath}
              echo "Dummy book removed (id=$book_id)."
            else
              echo "No dummy book found to remove."
            fi

            rm -f ${dummyBookPath}
            echo "Calibre library initialized successfully."
          else
            echo "Calibre library already exists, skipping initialization."
          fi
        '';
      };
    };

    services.calibre-web = {
      package = pkgs.calibre-web;
      user = "calibre-web";
      group = config.customGlobal.mediaGroup;
      dataDir = "/var/lib/calibre-web";
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
