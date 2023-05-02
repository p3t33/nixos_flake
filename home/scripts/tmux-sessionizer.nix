{ lib, pkgs, ... }:
let
  tmux-sessionizer = pkgs.writeShellScriptBin "tmux-sessionizer" ''
  SINGLE_ARGUMENT=1
DEFAULT_PATHS_TO_USE_IF_USER_NOT_PROVIDED_INTPUT="/home/$USER/projects /home/$USER "

_is_user_provided_directory_as_cli_arguments()
{
    [[ "$1" -eq $SINGLE_ARGUMENT ]]
}

_is_user_provided_his_working_directory_as_path_argument()
{
    [[ "." = $1 ]]
}

get_directory_to_open_as_tmux_session()
{
    if _is_user_provided_directory_as_cli_arguments "$#"; then
        if _is_user_provided_his_working_directory_as_path_argument $1; then
            echo "$(pwd)"
        else
            echo "$1"
        fi
    else
        find $DEFAULT_PATHS_TO_USE_IF_USER_NOT_PROVIDED_INTPUT -mindepth 1 -maxdepth 1 -type d | fzf
    fi
}


create_session_name()
{
    session_name=$(basename "$1" | tr . _)
    echo "$session_name"
}


_is_command_executed_from_within_tmux()
{
    tmux_pid=$(pgrep tmux)
    [[ -z $TMUX ]] && [[ -z $tmux_running ]]
}

# if attached session is created then the execution will continue inside of
# tmux session and if this is an issue both when command is executed inside
# of tmux session and from cli because this will create a nested session(double
# detach is one side effect).
create_new_detached_tmux_session()
{
    tmux new-session -ds$1 -c $2
}

_is_session_name_already_exit()
{
    # will return 1 if session does not exist
    tmux has-session -t=$1 2> /dev/null;
    return $?
}

start_tmux_session()
{
    if ! _is_command_executed_from_within_tmux; then
        if ! _is_session_name_already_exit $1; then
            create_new_detached_tmux_session $1 $2
        fi

        tmux switch-client -t $1
    else
        if ! _is_session_name_already_exit $1; then
            create_new_detached_tmux_session $1 $2
        fi

        tmux attach-session -t $1
    fi

}

switch_to_the_requsted_session()
{
    tmux switch-client -t $1
}


main()
{
    session_path=$(get_directory_to_open_as_tmux_session "$@")


    if [[ -z $session_path ]]; then
        echo "failed to get session path"
        exit 0
    fi

    session_name=$(create_session_name $session_path)

    start_tmux_session $session_name $session_path
}

main "$@"


  '';
in
{
    home.packages = [ tmux-sessionizer ];
}
