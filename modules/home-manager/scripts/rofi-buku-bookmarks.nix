{ pkgs, ... }:
let
  rofi-buku-bookmakrs = pkgs.writeShellScriptBin "rofi-buku-bookmakrs" ''
     # Function to list bookmarks
    list_bookmarks() {
     # List Buku bookmarks, format for Rofi
      buku -p -f 3 | sed 's/\t/ /g' | grep -v "buku: waiting for input"
    }

    # Function to open a bookmark
    open_bookmark() {
      local bookmark_id=$(echo "$1" | cut -d ' ' -f 1)
       if [ -n "$bookmark_id" ]; then
         buku -o "$bookmark_id"
       fi
    }

    # Main script logic
    if [ -z "$1" ]; then
      list_bookmarks
    else
      open_bookmark "$1"
    fi
 '';

in
{
  home.packages = [ rofi-buku-bookmakrs ];
}
