SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
MACHINES_DIR="$REPO_ROOT/machines"
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

validate_machine() {
  if [[ ! -d "$MACHINES_DIR/$MACHINE" ]]; then
    echo "Error: machine '$MACHINE' not found."
    echo ""
    echo "Available machines:"
    list_machines
    exit 1
  fi
}

validate_key() {
  if [[ ! -f "$KEY_PATH" ]]; then
    echo "Error: key file '$KEY_PATH' not found."
    exit 1
  fi
}
