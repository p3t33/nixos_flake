#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib.sh"

usage() {
  echo "Usage: $0 <machine-name> <key-path> <target-ip>"
  echo ""
  echo "Available machines:"
  list_machines
  exit 1
}

parse_args() {
  [[ $# -lt 3 ]] && usage

  MACHINE="$1"
  KEY_PATH="$2"
  TARGET_IP="$3"
}

prepare_extra_files() {
  EXTRA_FILES_DIR=$(mktemp -d)
  trap 'rm -rf "$EXTRA_FILES_DIR"' EXIT

  install -D -m644 "$KEY_PATH" "$EXTRA_FILES_DIR$SOPS_KEY_DEST"
}

deploy() {
  echo "Installing '$MACHINE' to root@$TARGET_IP"
  echo "Key: $KEY_PATH -> $SOPS_KEY_DEST"

  nix run github:nix-community/nixos-anywhere -- \
    --extra-files "$EXTRA_FILES_DIR" \
    --flake "$REPO_ROOT#$MACHINE" \
    --target-host "root@$TARGET_IP"
}

main() {
  parse_args "$@"
  validate_machine
  validate_key
  prepare_extra_files
  deploy
}

main "$@"
