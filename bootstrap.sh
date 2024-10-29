HOST_DIR_PATH="./machines/"
HOST_TO_INSTALL_AS_PROVIDED_BY_USER=$1
EXCLUSION_LIST=("work_pc" "home_desktop" "generic_linux_distro") # Add the hostnames to exclude
HOSTS_TO_CHOSE_FROM=$(find "$HOST_DIR_PATH" -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
MATCH_FOUND=0
HOST_TO_INSTALL=""

filter_directories() {
    readarray -t ALL_HOSTS <<< "$HOSTS_TO_CHOSE_FROM"
    FINAL_HOST_LIST=()

    for HOST in "${ALL_HOSTS[@]}"; do
        if [[ ! " ${EXCLUSION_LIST[*]} " =~ " $HOST " ]]; then
            FINAL_HOST_LIST+=("$HOST")
        fi
    done
}

check_host() {
    for DIR in "${FINAL_HOST_LIST[@]}"; do
        if [[ "$DIR" == "$HOST_TO_INSTALL_AS_PROVIDED_BY_USER" ]]; then
            MATCH_FOUND=1
            HOST_TO_INSTALL=$DIR
            break
        fi
    done
}

install_host() {
    if [[ $MATCH_FOUND -eq 1 ]]; then
        echo "Success: Argument matches the subdirectory '$HOST_TO_INSTALL'."
        sudo nix --experimental-features "nix-command flakes" run github:nix-community/disko -- --mode disko "./hosts/$HOST_TO_INSTALL/disko-config.nix"
        sudo cp /root/keys.txt /mnt
        sudo nixos-install --flake "../nixos_flake#$HOST_TO_INSTALL"
        exit 0 # success exit code
    else
        echo "Failure: Argument does not match any valid hosts."
        echo "Available hosts for bootstrap installation in $HOST_DIR_PATH are:"
        for DIR in "${FINAL_HOST_LIST[@]}"; do
            echo "  - $DIR"
        done
        exit 1 # failure exit code
    fi
}

main() {
    filter_directories
    check_host
    install_host
}

main "$@"
