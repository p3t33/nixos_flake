#!/usr/bin/env bash
set -euo pipefail

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
  echo "Usage: $0 <machine-name> <key-path> <target-ip>"
  echo ""
  echo "Available machines:"
  list_machines
  exit 1
}

[[ $# -lt 3 ]] && usage

MACHINE="$1"
KEY_PATH="$2"
TARGET_IP="$3"

# Validate machine exists
if [[ ! -d "$MACHINES_DIR/$MACHINE" ]]; then
  echo "Error: machine '$MACHINE' not found."
  echo ""
  echo "Available machines:"
  list_machines
  exit 1
fi

# Validate key file exists
if [[ ! -f "$KEY_PATH" ]]; then
  echo "Error: key file '$KEY_PATH' not found."
  exit 1
fi

# Create temp directory with proper structure
temp=$(mktemp -d)
trap "rm -rf $temp" EXIT

install -D -m644 "$KEY_PATH" "$temp$SOPS_KEY_DEST"

echo "Installing '$MACHINE' to root@$TARGET_IP"
echo "Key: $KEY_PATH -> $SOPS_KEY_DEST"

nix run github:nix-community/nixos-anywhere -- \
  --extra-files "$temp" \
  --flake ".#$MACHINE" \
  --target-host "root@$TARGET_IP"
