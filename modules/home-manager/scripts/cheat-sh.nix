{ lib, pkgs, ... }:
let
  cheat-sh = pkgs.writeShellScriptBin "cheat-sh" ''
  languages=$(echo "cpp python bash c go" | tr ' ' '\n')
core_utils=$(echo "find mv sed awk tar grep fd rsync ssh scp nmcli" | tr ' ' '\n')

get_user_selection_from_a_list() {
    printf "$languages\n$core_utils" | fzf
}

get_query() {
    read -p "query: " query
    echo $query
}

is_selected_a_language() {
    local selected=$1
    printf "$languages" | grep -qs $selected
}

_is_command_executed_from_within_tmux()
{
    [[ -n $TMUX ]]
}

create_tmux_window() {
    local selected=$1
    local query=$2
    local block_until_user_presses_enter="read -p 'Press [Enter] key to exit...'; echo; "

    if is_selected_a_language $selected; then
        if _is_command_executed_from_within_tmux; then
            tmux neww bash -c "curl cht.sh/$selected/$query; $block_until_user_presses_enter"
        else
            curl "cht.sh/$selected/$query";
        fi
    else
        if _is_command_executed_from_within_tmux; then
            tmux neww bash -c "curl -s cht.sh/$selected~$query; $block_until_user_presses_enter"
        else
            curl "cht.sh/$selected~$query";
        fi
    fi
}

main() {
    local selected=$(get_user_selection_from_a_list)
    local query=$(get_query)
    create_tmux_window $selected $query
}

main

  '';
in
{
    home.packages = [ cheat-sh ];
}

