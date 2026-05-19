#!/usr/bin/env bash
set -euo pipefail

# Run this script from a NixOS live CD with the flake cloned/available.
# It partitions disks, places the sops age key, and installs NixOS.
#
# Usage: ./bootstrap.sh <machine-name> <key-path>

MACHINES_DIR="./machines"
SOPS_KEY_DEST="/etc/sops/age/keys.txt"
EXCLUSIONS=("generic_linux_distro")

list_machines() {
  for dir in "$MACHINES_DIR"/*/; do
    name=$(basename "$dir")
    excluded=false
    for exc in "${EXCLUSIONS[@]}"; do
      [[ "$name" == "$exc" ]] && excluded=true && break
    done
    $excluded || echo "  - $name"
  done
}

usage() {
  echo "Usage: $0 <machine-name> <key-path>"
  echo ""
  echo "Available machines:"
  list_machines
  exit 1
}

[[ $# -lt 2 ]] && usage

MACHINE="$1"
KEY_PATH="$2"

if [[ ! -d "$MACHINES_DIR/$MACHINE" ]]; then
  echo "Error: machine '$MACHINE' not found."
  echo ""
  echo "Available machines:"
  list_machines
  exit 1
fi

if [[ ! -f "$KEY_PATH" ]]; then
  echo "Error: key file '$KEY_PATH' not found."
  exit 1
fi

echo "Partitioning disks for '$MACHINE'..."
sudo nix --experimental-features "nix-command flakes" \
  run github:nix-community/disko -- --mode disko "$MACHINES_DIR/$MACHINE/disko-configuration.nix"

echo "Placing sops age key..."
sudo install -D -m644 "$KEY_PATH" "/mnt$SOPS_KEY_DEST"

echo "Installing NixOS..."
sudo nixos-install --flake ".#$MACHINE"

echo "Done. Reboot when ready."
