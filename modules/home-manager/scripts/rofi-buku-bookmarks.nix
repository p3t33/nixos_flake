{ config, lib, pkgs, ... }:
let
  cfg = config.programs.rofi;
  # Define the focusing logic as a reusable fragment
  focus_browser_logic = ''
    # Give the browser time to open the URL
    sleep 0.1

    # Find the browser window and focus it (using i3-msg or wmctrl)
    # Note: Using 'Navigator.Firefox' requires the browser to be Firefox
    windowid=$(wmctrl -lx | grep -i Navigator.Firefox | awk '{print $1}' | tail -1)
    if [ -n "$windowid" ]; then
      i3-msg "[id=\"$windowid\"] focus" > /dev/null &
    fi
  '';

  rofi-buku-bookmakrs = pkgs.writeShellScriptBin "rofi-buku-bookmakrs" ''
    # Function to perform a Google search
    google_search() {
      local query="$1"
      # URL encode spaces to '+'
      local url_query=$(echo "$query" | sed 's/ /+/g')

      # Open the search in the default browser
      xdg-open "https://www.google.com/search?q=$url_query" &

      # Add the focusing logic here
      ${focus_browser_logic}
    }

    # Function to list bookmarks based on the filter provided by Rofi ($1)
    list_bookmarks() {
      local filter="$1"
      local search_command="buku -p -f 3"

      if [ -n "$filter" ]; then
        search_command="buku -s \"$filter\" -p -f 3"
      fi

      # List Buku bookmarks, format for Rofi
      BOOKMARKS=$(eval "$search_command" | sed 's/\t/ /g' | grep -v "buku: waiting for input")

      echo "$BOOKMARKS"
    }

    # Function to open a bookmark or perform a Google search
    open_action() {
      local filter="$1"

      # 1. Try to get a bookmark ID from the input
      local bookmark_id=$(echo "$filter" | cut -d ' ' -f 1)

      if [[ "$bookmark_id" =~ ^[0-9]+$ ]]; then
        # Found a valid-looking numeric ID, open the bookmark
        buku -o "$bookmark_id"

        # Focus logic (original bookmark focus)
        ${focus_browser_logic}

      else
        # If the input is NOT a bookmark ID, treat it as a search query
        google_search "$filter"
      fi
    }

    # Main script logic
    if [ -z "$1" ]; then
      # Phase 1: Rofi starts - get the full list
      list_bookmarks
    else
      # If ENTER is pressed, try to perform an action (bookmark or search)
      open_action "$1"
    fi
  '';

in
{
  config = lib.mkIf cfg.enable {
    home.packages = [ rofi-buku-bookmakrs ];
  };
}
