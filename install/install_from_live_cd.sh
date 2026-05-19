#!/usr/bin/env bash
set -euo pipefail

# Run this script from a NixOS live CD with the flake cloned/available.
# It partitions disks, places the sops age key, and installs NixOS.
#
# Usage: ./install_from_live_cd.sh <machine-name> <key-path>

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib.sh"

usage() {
  echo "Usage: $0 <machine-name> <key-path>"
  echo ""
  echo "Available machines:"
  list_machines
  exit 1
}

parse_args() {
  [[ $# -lt 2 ]] && usage

  MACHINE="$1"
  KEY_PATH="$2"
}

partition_disks() {
  echo "Partitioning disks for '$MACHINE'..."
  sudo nix --experimental-features "nix-command flakes" \
    run github:nix-community/disko -- --mode disko "$MACHINES_DIR/$MACHINE/disko-configuration.nix"
}

place_sops_key() {
  echo "Placing sops age key..."
  sudo install -D -m644 "$KEY_PATH" "/mnt$SOPS_KEY_DEST"
}

install_nixos() {
  echo "Installing NixOS..."
  sudo nixos-install --no-root-passwd --flake "$REPO_ROOT#$MACHINE"
}

main() {
  parse_args "$@"
  validate_machine
  validate_key
  partition_disks
  place_sops_key
  install_nixos
  echo "Done. Reboot when ready."
}

main "$@"
