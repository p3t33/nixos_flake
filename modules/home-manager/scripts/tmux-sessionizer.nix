{ lib, config, pkgs, ... }:
let
  cfg = config.programs.tmux;
  tmux-sessionizer = pkgs.writeShellScriptBin "tmux-sessionizer" ''
    SINGLE_ARGUMENT=1
    state_directory="''${XDG_STATE_HOME:-$HOME/.local/state}/tmux"
    state_file="$state_directory/session-order"

    list_ordered_sessions()
    {
        local session
        local -a existing=() saved=()
        local -A remaining=()

        mapfile -t existing < <(${lib.getExe pkgs.tmux} -N list-sessions -F '#{session_name}' 2>/dev/null | LC_ALL=C ${lib.getExe' pkgs.coreutils "sort"})
        for session in "''${existing[@]}"; do
            remaining["$session"]=1
        done

        if [[ -f $state_file ]]; then
            mapfile -t saved < "$state_file"
        else
            # Bootstrap from live activity until the first access is recorded.
            mapfile -t saved < <(${lib.getExe pkgs.tmux} -N list-sessions -F $'#{session_activity}\t#{session_name}' 2>/dev/null | LC_ALL=C ${lib.getExe' pkgs.coreutils "sort"} -t $'\t' -k1,1nr -k2,2 | ${lib.getExe pkgs.gawk} -F '\t' '{print $2}')
        fi

        for session in "''${saved[@]}" "''${existing[@]}"; do
            if [[ -n $session && -n ''${remaining["$session"]:-} ]]; then
                printf '%s\n' "$session"
                unset 'remaining[$session]'
            fi
        done
    }

    save_session_order()
    (
        set -e
        umask 077
        local name=$1 previous_name=$2 promote=$3 session temporary
        local -a saved=()

        ${lib.getExe' pkgs.coreutils "mkdir"} -p -- "$state_directory"
        exec 9>"$state_directory/session-order.lock"
        ${lib.getExe' pkgs.util-linux "flock"} 9

        if [[ -f $state_file ]]; then
            mapfile -t saved < "$state_file"
        else
            mapfile -t saved < <(list_ordered_sessions)
        fi

        temporary=$(${lib.getExe' pkgs.coreutils "mktemp"} "$state_file.XXXXXX")
        trap '${lib.getExe' pkgs.coreutils "rm"} -f -- "$temporary"' EXIT
        {
            if [[ $promote == true ]]; then
                printf '%s\n' "$name"
            fi
            for session in "''${saved[@]}"; do
                if [[ $session == "$previous_name" ]]; then
                    if [[ $promote != true ]]; then
                        printf '%s\n' "$name"
                    fi
                elif [[ -n $session && $session != "$name" ]]; then
                    printf '%s\n' "$session"
                fi
            done
        } > "$temporary"
        ${lib.getExe' pkgs.coreutils "mv"} -f -- "$temporary" "$state_file"
    )

    remember_session_name()
    {
        local name
        name=$(${lib.getExe pkgs.tmux} -N display-message -p -t "$1:" '#{session_name}') || return
        [[ -n $name ]] || return 1
        ${lib.getExe pkgs.tmux} -N set-option -t "$1" @sessionizer-name "$name"
    }

    remember_session_names()
    {
        local session
        while IFS= read -r session; do
            remember_session_name "$session" || true
        done < <(${lib.getExe pkgs.tmux} -N list-sessions -F '#{session_id}' 2>/dev/null)
    }

    record_session()
    {
        local name restore_pid hook_restore_pid=''${2:-}
        restore_pid=$(${lib.getExe pkgs.tmux} -N show-options -gqv @sessionizer-restore-pid) || return
        # A restore event may reach this helper after the restore marker is cleared.
        if [[ -n $hook_restore_pid && $hook_restore_pid != "$restore_pid" ]]; then
            return 0
        fi
        if [[ $restore_pid == __sessionizer_restore_* ]] ||
           { [[ $restore_pid =~ ^[1-9][0-9]*$ ]] && kill -0 "$restore_pid" 2>/dev/null; }; then
            return 0
        fi
        name=$(${lib.getExe pkgs.tmux} -N display-message -p -t "$1:" '#{session_name}') || return
        [[ -n $name ]] || return 1
        save_session_order "$name" "$name" true
    }

    finish_restore()
    {
        local restore_pid session name barrier
        local -a sessions=()
        restore_pid=$(${lib.getExe pkgs.tmux} -N show-options -gqv @sessionizer-restore-pid) || return
        [[ $restore_pid == "$1" ]] || return 0

        mapfile -t sessions < <(${lib.getExe pkgs.tmux} -N list-sessions -F '#{session_id}')
        if (( ''${#sessions[@]} == 0 )); then
            ${lib.getExe pkgs.tmux} -N set-option -gu @sessionizer-restore-pid
            return
        fi
        session=''${sessions[0]}
        name=$(${lib.getExe pkgs.tmux} -N display-message -p -t "$session:" '#{session_name}') || return
        barrier="__sessionizer_restore_''${restore_pid}_''${BASHPID}_$RANDOM"

        # The rename notification runs after earlier synchronous access hooks.
        # Restore the name in the same command list, before notifications run.
        ${lib.getExe pkgs.tmux} -N set-option -g @sessionizer-restore-pid "$barrier" \; \
            rename-session -t "$session" -- "$barrier" \; \
            rename-session -t "$session" -- "''${name//#/##}"
    }

    wait_for_restore()
    {
        local restore_pid remaining deadline=$((SECONDS + 30))
        while true; do
            restore_pid=$(${lib.getExe pkgs.tmux} -N show-options -gqv @sessionizer-restore-pid) || return
            if [[ $restore_pid != __sessionizer_restore_* ]] &&
               ! { [[ $restore_pid =~ ^[1-9][0-9]*$ ]] && kill -0 "$restore_pid" 2>/dev/null; }; then
                return 0
            fi
            remaining=$((deadline - SECONDS))
            if (( remaining <= 0 )); then
                printf 'Timed out waiting for tmux restoration\n' >&2
                return 1
            fi
            if [[ $restore_pid == __sessionizer_restore_* ]]; then
                ${lib.getExe' pkgs.coreutils "timeout"} "$remaining" ${lib.getExe pkgs.tmux} -N wait-for "$restore_pid" || return
            else
                ${lib.getExe' pkgs.coreutils "sleep"} 0.05
            fi
        done
    }

    rename_recorded_session()
    {
        local name previous_name
        name=$(${lib.getExe pkgs.tmux} -N display-message -p -t "$1:" '#{session_name}') || return
        previous_name=$(${lib.getExe pkgs.tmux} -N show-options -qv -t "$1" @sessionizer-name) || return
        if [[ -n $previous_name && $previous_name != "$name" && -f $state_file ]]; then
            save_session_order "$name" "$previous_name" false || return
        fi
        remember_session_name "$1"
    }

    attach_to_session()
    {
        if _is_command_executed_from_within_tmux; then
            ${lib.getExe pkgs.tmux} switch-client -t "=$1"
        else
            ${lib.getExe pkgs.tmux} attach-session -t "=$1"
        fi
    }

    switch_previous_session()
    {
        local client=$1 current_name session
        current_name=$(${lib.getExe pkgs.tmux} -N display-message -p -t "$2:" '#{session_name}') || return
        [[ -n $current_name ]] || return 1

        while IFS= read -r session; do
            if [[ $session != "$current_name" ]]; then
                ${lib.getExe pkgs.tmux} -N switch-client -c "$client" -t "=$session"
                return
            fi
        done < <(list_ordered_sessions)

        ${lib.getExe pkgs.tmux} -N display-message -c "$client" 'No other session'
    }

    select_last_session()
    (
        umask 077
        local selector

        ${lib.getExe' pkgs.coreutils "mkdir"} -p -- "$state_directory" || return
        exec 9>"$state_directory/session-launch.lock" || return
        ${lib.getExe' pkgs.util-linux "flock"} 9 || return

        # Keep the initial client connected while configuration and selection run.
        printf -v selector '%q --select-last-session' "$0"
        ${lib.getExe pkgs.tmux} start-server \; run-shell "$selector" 9>&-
    )

    select_existing_or_create_session()
    {
        local -a sessions=()
        wait_for_restore || return
        mapfile -t sessions < <(list_ordered_sessions)
        if (( ''${#sessions[@]} == 0 )); then
            ${lib.getExe pkgs.tmux} new-session -d -P -F '#{session_name}'
        else
            printf '%s\n' "''${sessions[0]}"
        fi
    }

    attach_last_session()
    {
        local session
        session=$(select_last_session) || return
        attach_to_session "$session"
    }

    _is_user_provided_directory_as_cli_arguments()
    {
        [[ "$1" -eq $SINGLE_ARGUMENT ]]
    }

    _is_user_provided_his_working_directory_as_path_argument()
    {
        [[ "." = "$1" ]] || [[ "./" = "$1" ]]
    }

    _is_user_provided_name_of_exisitng_directory()
    {
        [[ -d $1 ]]
    }

    __get_existing_tmux_sessions()
    {
        local session
        while IFS= read -r session; do
            printf '\033[1;34m%s\033[0m\n' "$session"
        done < <(list_ordered_sessions)
    }

    _generate_list_of_existing_sessions_and_most_frequently_accessed_paths()
    {
        # Will create ordered list of both existing sessions and and most
        # frequently accessed paths with existing sessins in color and first in
        # the order.
        {
            __get_existing_tmux_sessions
            ${lib.getExe pkgs.zoxide} query --list | while IFS= read -r directory || [[ -n $directory ]]; do
                (CDPATH="" cd -- "$directory" >/dev/null 2>&1) || continue
                printf '%s\n' "$directory"
            done
        } | ${lib.getExe pkgs.fzf} --ansi --no-sort

    }

    get_directory_to_open_as_tmux_session()
    {
        local directory
        if _is_user_provided_directory_as_cli_arguments "$#"; then
            if _is_user_provided_his_working_directory_as_path_argument "$1"; then
                directory=$(pwd) || return 1
            else
                if ! _is_user_provided_name_of_exisitng_directory "$1"; then
                    if ! mkdir -p -- "$1"; then
                        printf 'Cannot create directory: %s\n' "$1" >&2
                        return 1
                    fi
                fi
                directory=$1
            fi
            if ! (CDPATH="" cd -- "$directory" >/dev/null 2>&1); then
                printf 'Cannot access directory: %s\n' "$directory" >&2
                return 1
            fi
            printf '%s\n' "$directory"
        else
            _generate_list_of_existing_sessions_and_most_frequently_accessed_paths
        fi
    }


    create_session_name()
    {
        session_name=$(basename -- "$1" | tr . _)
        echo "$session_name"
    }


    _is_command_executed_from_within_tmux()
    {
        [[ -n ''${TMUX:-} ]]
    }

    # if attached session is created then the execution will continue inside of
    # tmux session and if this is an issue both when command is executed inside
    # of tmux session and from cli because this will create a nested session(double
    # detach is one side effect).
    create_new_detached_tmux_session()
    {
        ${lib.getExe pkgs.tmux} new-session -d -s "$1" -c "$2"
    }

    _is_session_name_already_exit()
    {
        # will return 1 if session does not exist
        ${lib.getExe pkgs.tmux} -N has-session -t="$1" 2> /dev/null;
        return $?
    }

    start_tmux_session()
    {
        if ! _is_session_name_already_exit "$1"; then
            create_new_detached_tmux_session "$1" "$2" || return
        fi
        attach_to_session "$1"
    }


    main()
    {
        case "''${1:-}" in
            --switch-previous)
                if (( $# != 3 )); then
                    printf 'Usage: tmux-sessionizer --switch-previous CLIENT SESSION\n' >&2
                    return 1
                fi
                switch_previous_session "$2" "$3"
                return
                ;;
            --record)
                if (( $# < 2 || $# > 3 )); then
                    printf 'Usage: tmux-sessionizer --record SESSION [RESTORE_PID]\n' >&2
                    return 1
                fi
                record_session "$2" "''${3:-}"
                return
                ;;
            --remember-name|--rename|--finish-restore)
                if (( $# != 2 )); then
                    printf 'Usage: tmux-sessionizer %s SESSION\n' "$1" >&2
                    return 1
                fi
                case "$1" in
                    --remember-name) remember_session_name "$2" ;;
                    --rename) rename_recorded_session "$2" ;;
                    --finish-restore) finish_restore "$2" ;;
                esac
                return
                ;;
            --remember-names|--list|--attach-last|--select-last-session)
                (( $# == 1 )) || return 1
                case "$1" in
                    --remember-names) remember_session_names ;;
                    --list) list_ordered_sessions ;;
                    --attach-last) attach_last_session ;;
                    --select-last-session) select_existing_or_create_session ;;
                esac
                return
                ;;
        esac

        if ! session_path=$(get_directory_to_open_as_tmux_session "$@"); then
            (( $# == 0 )) || return 1
        fi

        if [[ -z $session_path ]]; then
            echo "failed to get session path"
            exit 0
        fi

        if (( $# == 0 )) && _is_session_name_already_exit "$session_path"; then
            attach_to_session "$session_path"
        else
            session_name=$(create_session_name "$session_path")
            start_tmux_session "$session_name" "$session_path"
        fi
    }

    main "$@"

  '';
in
{
  options.custom.scripts.tmuxSessionizer.package = lib.mkOption {
    type = lib.types.package;
    readOnly = true;
    internal = true;
    default = tmux-sessionizer;
    description = "Tmux session picker and persistent last-used ordering helper.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ tmux-sessionizer ];
  };
}
