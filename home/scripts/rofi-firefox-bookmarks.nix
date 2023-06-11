{ lib, pkgs, ... }:
let
  rofi-firefox-bookmakrs = pkgs.writeShellScriptBin "rofi-firefox-bookmakrs" ''
# places.sqlite location
    places_file="$(find ~/.mozilla/firefox/*.default*/ -name "places.sqlite" -print -quit)"

# places.sqlite copy
    places_backup="$(dirname "$places_file")/places.rofi.sqlite"

# path to the sqlite3 binary
    sqlite_path="$(which sqlite3)"

# sqlite3 parameters (define separator character)
    sqlite_params="-separator ^"

# browser path
    browser_path="$(which firefox)"

# functions

# create a backup file
    create_backup() {
        if [ "$#" -eq 2 ] && [ -n "$1" ] && [ -n "$2" ]; then
            if [ ! -f "$2" ] || [ "$1" -nt "$2" ]; then
                cp "$1" "$2"
                    fi
                    fi
    }

# process bookmarks
process_bookmarks() {
    query="select b.title, p.url, b.id, SUBSTR(SUBSTR(p.url, INSTR(url, '//') + 2), 0, INSTR(SUBSTR(p.url, INSTR(p.url, '//') + 2), '/')) as domain from moz_bookmarks as b left outer join moz_places as p on b.fk=p.id where b.type = 1 and p.hidden=0 and b.title not null" #  and parent=$1
        $sqlite_path $sqlite_params "$places_backup" "$query" | while IFS=^ read title url id domain; do
        if [ -z "$title" ]; then
            title="$url"
                fi
                printf "%-500s {id:%s}\n" "$title [$domain]" "$id"
                done
}

# process bookmark
process_bookmark() {
    if [ "$#" = 1 ] && [ -n "$1" ]; then
        id="$(echo $1 | sed "s|.*{id:\(.*\)}$|\1|")"
            query="select p.url from moz_bookmarks as b left outer join moz_places as p on b.fk=p.id where b.type = 1 and p.hidden=0 and b.title not null and b.id=$id"
            url="$($sqlite_path $sqlite_params "$places_backup" "$query")"
            nohup $browser_path "$url" >/dev/null 2>&1 &

            # give Firefox time to open the window to be found by wmctrl, if time is too
            # short them wmctrl will fail and i3-msg will fail to swtich.
            sleep 0.1
            windowid=$(wmctrl -lx | grep -i Navigator.Firefox | awk '{print $1}' | tail -1)

            # & is used for the script not to wait for to command to finish
            # /dev/null is used to discard the standard output (stdout) of a command.
            # It prevents the output of the command from being displayed in the
            # terminal or being passed to other parts of the script.
            # this command is i3wm specific!
            i3-msg "[id=\"$windowid\"] focus" > /dev/null &
            fi
}

# application

parameter="$1"

# create a backup, as we cannot operate on a places.sqlite file directly due to exclusive lock
create_backup "$places_file" "$places_backup"

# open a bookmark when there is a param sety
if [ -n "$parameter" ]; then
process_bookmark "$parameter"
exit
fi

# process bookmarks
process_bookmarks

  '';
in
{
    home.packages = [rofi-firefox-bookmakrs ];
}
